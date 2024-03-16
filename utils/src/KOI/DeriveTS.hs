module KOI.DeriveTS where

import Control.Monad(guard)
import Control.Applicative((<|>))
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map(Map)
import KOI.PP
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Aeson qualified as JS
import Data.Aeson.Key qualified as JS

data TSDecl = TSDecl
  { tsName :: Text
  , tsParams :: [Text]
  , tsDef :: TSType
  }

data TSType =
    TSNamed Text [TSType]
  | TSLit Text
  | TSUnion [TSType]
  | TSTuple [TSType]
  | TSRecord [(Text, TSType)]
  | TSArray TSType
  | TSMap TSType TSType   -- only some types can be keys

tsString, tsNumber, tsBool, tsNull :: TSType
tsString = TSNamed "string" []
tsNumber = TSNamed "number" []
tsBool   = TSNamed "boolean" []
tsNull   = TSNamed "null" []

tsMaybe :: TSType -> TSType
tsMaybe t = TSUnion [ t, tsNull ]

instance PP TSDecl where
  pp d =
    ("type" <+> pp (tsName d) <.> ps <+> "=") $$ nest 2 (pp (tsDef d))
    where ps = case tsParams d of
                 [] -> mempty
                 ts -> hcat [ "<", commaSep (map pp ts), ">" ]

instance PP TSType where
  pp ty =
    case ty of
      TSNamed x ps ->
        pp x <.>
        case ps of
          [] -> mempty
          _  -> hcat [ "<", commaSep (map pp ps), ">" ]
      TSLit txt   -> pp (show txt)
      TSUnion ts  -> block " " "|" mempty (map pp ts)
      TSTuple ts  -> block "[" "," "]" (map pp ts)
      TSRecord fs -> block "{" "," "}" (map doField fs)
        where doField (x,t) = pp x <.> ":" <+> pp t

      TSArray t -> wrap (pp t) <.> "[]"
        where wrap = case t of
                       TSUnion {} -> parens
                       _ -> id

      TSMap tk tv ->
        "{" <+> brackets ("x:" <+> pp tk) <.> ":" <+> pp tv <+> "}"
    where
    block open sep close xs
      | null xs = open <.> close
      | otherwise =
        vcat (zipWith (<+>) (open  : repeat sep) xs) $$ close

--------------------------------------------------------------------------------

data TypeDef = TypeDef
  { typeTS        :: TSType
  , typeToJSON    :: Q (Pat, Exp)
  , typeFromJSON  :: Q Exp -> Q Exp
  }


fromName :: Name -> Text
fromName (Name (OccName s) _) = Text.pack s

typeToTS :: Type -> Either String TSType
typeToTS ty = doApp ty []
  where
  unsupported = Left "Unsupported type."

  doAppT f x ts =
    do t <- typeToTS x
       doApp f (t : ts)

  doCon c args
    | c == ''Bool       = pure tsBool
    | c == ''String     = pure tsString
    | c == ''Text       = pure tsString
    | c == ''Int        = pure tsNumber
    | c == ''Integer    = pure tsNumber
    | c == ''Maybe
    , [t] <- args       = pure (tsMaybe t)
    | c == ''Map
    , [k,v] <- args     = pure (TSMap k v)
    | otherwise         = pure (TSNamed (fromName c) args)

  doApp f args =
    case f of
      AppT g x       -> doAppT g x args
      AppKindT g _   -> doApp g args
      SigT g _       -> doApp g args
      ParensT g      -> doApp g args

      VarT a
        | null args     -> pure (TSNamed (fromName a) [])

      ConT c         -> doCon c args
      TupleT _       -> pure (TSTuple args)
      ListT
        | [t] <- args   -> pure (TSArray t)

      _                 -> unsupported


exportFlatType :: Name -> Q [Dec]
exportFlatType = exportType' True

exportType :: Name -> Q [Dec]
exportType = exportType' False

exportType' :: Bool -> Name -> Q [Dec]
exportType' flat x =
  do nd <- reifyDatatype x
     declToTS flat nd


declToTS :: Bool -> DatatypeInfo -> Q [Dec]
declToTS flat nd =
  case mapM (doConDef untagged) cons of
    Left err -> fail err
    Right defs ->
      do ts <- toTS (map typeTS defs)
         tJ <- toJS (map typeToJSON defs)
         fJ <- fromJS (map typeFromJSON defs)
         pure (tJ : fJ : ts)
  where
  cons     = datatypeCons nd
  untagged = flat
          || case datatypeCons nd of
               [_]  -> True
               _    -> all (null . constructorFields) cons

  toTS :: [TSType] -> Q [Dec]
  toTS ts =
    do dS <- sigD name [t| String |]
       let txt = show (pp d)
       dE <- mkD name [e| txt |]
       pure [dS,dE]
    where
    name = mkName ("tys" ++ nameBase (datatypeName nd))
    d = TSDecl
          { tsName   = fromName (datatypeName nd)
          , tsParams = map (fromName . tvName) (datatypeVars nd)
          , tsDef =
              case ts of
                  [t] -> t
                  _     -> TSUnion ts
         }

  toJS :: [Q (Pat,Exp)] -> Q Dec
  toJS ms =
    instanceD (pure ctx) (pure (jsC (datatypeType nd)))
      [ mkD 'JS.toJSON [| \x -> $(caseE [| x |] (map mkM ms)) |] ]
    where
    jsC t = classPred ''JS.ToJSON [t]
    ctx   = [ jsC (VarT (tvName a)) | a <- datatypeVars nd ]
    mkM m = m >>= \(p,e) -> match (pure p) (normalB (pure e)) []

  fromJS :: [Q Exp -> Q Exp] -> Q Dec
  fromJS xs =
    instanceD (pure ctx) (pure (jsC (datatypeType nd)))
      [ mkD 'JS.parseJSON [| \x -> $(alts [| x |]) |] ]
    where
    jsC t     = classPred ''JS.FromJSON [t]
    ctx       = [ jsC (VarT (tvName a)) | a <- datatypeVars nd ]
    alts x    = foldr1 jn [ f x | f <- xs ]
    jn f g    = [| $f <|> $g |]

  mkD x e = valD (varP x) (normalB e) []

--------------------------------------------------------------------------------
doConDef :: Bool -> ConstructorInfo -> Either String TypeDef
doConDef untagged con = makeDef <$> mapM typeToTS fields
  where

  -- Tag+Contents encoding
  addTag def =
    TypeDef
      { typeTS = TSRecord [ ("tag", TSLit conTag)
                          , ("contents", typeTS def)
                          ]
      , typeToJSON =
          do (p,e) <- typeToJSON def
             newE <- [| JS.object [ "tag" JS..= (conTag :: Text)
                                  , "contents" JS..= $(pure e)
                                  ]
                      |]
             pure (p,newE)

      , typeFromJSON = \j ->
          [| JS.withObject (Text.unpack conTag) (\obj ->
               do tag <- obj JS..: "tag"
                  guard (tag == (conTag :: Text))
                  ctr <- obj JS..: "contents"
                  $(typeFromJSON def [| ctr |]))
             $j
           |]
      }

  makeDef ts =
    case ts of

      -- No fields
      []

        -- Key encoding
        | untagged ->
            TypeDef
              { typeTS = TSLit conTag
              , typeToJSON =
                 do (p,_) <- lhsPat
                    e     <- [| JS.String conTag |]
                    pure (p, e)


              , typeFromJSON = \j ->
                 [| JS.withText (Text.unpack conTag) (\txt ->
                      do guard (txt == conTag)
                         pure $conFun)
                    $j
                  |]
              }

        -- Tag + contents
        | otherwise ->
            addTag
              TypeDef
                { typeTS = TSTuple []
                , typeToJSON =
                    do (p,_) <- lhsPat
                       e     <- qToJson [| () |]
                       pure (p,e)
                , typeFromJSON = const [| pure $conFun |]
                }

      -- Single field
      [t] ->
        (if untagged then id else addTag)
        TypeDef
          { typeTS = t

          , typeToJSON =
              do (p,xs) <- lhsPat
                 e      <- qToJson (head xs)
                 pure (p, e)

          , typeFromJSON = \j -> [| $conFun <$> $(qParseJson j) |]
          }

      -- Multiple fields
      _ ->
        (if untagged then id else addTag)
        case constructorVariant con of

          -- Record
          RecordConstructor fs ->
            TypeDef
              { typeTS = TSRecord (zip labs ts)

              , typeToJSON =
                  do (p,xs) <- lhsPat
                     let mk l x = [| JS.fromText l JS..= $x |]
                     e <- [| JS.object $(listE (zipWith mk labs xs)) |]
                     pure (p,e)

              , typeFromJSON = \j ->
                  do xs <- fieldVars
                     let fromObj    = [| \obj -> $(doE (stmts [| obj |])) |]
                         stmts obj  = zipWith getF labs xs ++
                                      [noBindS [| pure $(conFunApp xs) |]]
                           where
                           getF l x = bindS (varP x) [| $obj JS..: JS.fromText l |]

                     [| JS.withObject "obj" $fromObj $j |]
              }
            where
            labs = map fromName fs


          -- Tuple
          _ ->
            TypeDef
              { typeTS = TSTuple ts

              , typeToJSON =
                  do (p,xs) <- lhsPat
                     e      <- qToJson (tupE xs)
                     pure (p, e)

              , typeFromJSON = \j ->
                  do xs <- fieldVars
                     [| do $(tupP (map varP xs)) <- $(qParseJson j)
                           pure $(conFunApp xs)
                      |]
              }

  fields    = constructorFields con
  conTag    = fromName (constructorName con)
  conFun    = conE (constructorName con)
  conFunApp = foldl appE conFun . map varE
  fieldVars = mapM (const (newName "x")) fields
  lhsPat =
    do xs <- fieldVars
       p  <- conP (constructorName con) (map varP xs)
       pure (p, map varE xs)

  qToJson    = appE [| JS.toJSON |]
  qParseJson = appE [| JS.parseJSON |]


