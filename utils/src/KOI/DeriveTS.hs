module KOI.DeriveTS where

import Control.Monad(guard)
import Control.Applicative((<|>))
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import Text.Read(readMaybe)
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Aeson qualified as JS
import Data.Aeson.Key qualified as JS
import Data.Aeson.Types qualified as JS

import KOI.PP


type TSModules = Map Text (Map Text (Map Text (TSDecl Name)))

addDecl :: TSDecl Name -> TSModules -> Maybe TSModules
addDecl d mp =
  do pkg <- namePackage name
     mo  <- nameModule name
     pure (Map.union (Map.singleton (Text.pack pkg)
                     (Map.singleton (Text.pack mo)
                     (Map.singleton (Text.pack (nameBase name)) d))) mp)
  where
  name = tsName d

-- | Returns a map associating Haskell (pkg,module) names with TS module names
moduleNames :: TSModules -> Map (Text,Text) Text
moduleNames db =
  Map.fromList $
  concatMap pickName $
  Map.toList $
  Map.fromListWith (++)
  [ (m,[p]) | (p,ms) <- Map.toList (Map.keys <$> db), m <- ms ]

  where
  pickName (m,ps) =
    case ps of
      [p] -> [ ((p,m), m)]
      _   -> [ ((p,m), p <> "_" <> m) | p <- ps ]


--------------------------------------------------------------------------------


data TSModule = TSModule
  { tsmName    :: Text
  , tsmImports :: [TSImport]
  }

data TSImport = TSImport
  { tsiModule :: Text
  , tsiEntity :: [(Text,Maybe Text)]
  }

data TSDecl name = TSDecl
  { tsName   :: name
  , tsParams :: [Text]
  , tsDef    :: TSType name
  }

data TSType name =
    TSNamed name [TSType name]
  | TSBuiltIn Text
  | TSLit Text
  | TSUnion [TSType name]
  | TSTuple [TSType name]
  | TSRecord [(Text, TSType name)]
  | TSArray (TSType name)
  | TSMap (TSType name) (TSType name)   -- only some types can be keys

tsString, tsNumber, tsBool, tsNull :: TSType name
tsString = TSBuiltIn "string"
tsNumber = TSBuiltIn "number"
tsBool   = TSBuiltIn "boolean"
tsNull   = TSBuiltIn "null"

tsMaybe :: TSType name -> TSType name
tsMaybe t = TSUnion [ t, tsNull ]
--------------------------------------------------------------------------------

tsDeclDeps :: Ord name => TSDecl name -> Set name
tsDeclDeps = tsDeps . tsDef

tsDeps :: Ord name => TSType name -> Set name
tsDeps ty =
  case ty of
    TSNamed x ts -> Set.insert x (list ts)
    TSBuiltIn {} -> mempty
    TSLit {}     -> mempty
    TSUnion ts   -> list ts
    TSTuple ts   -> list ts
    TSRecord ts  -> list (map snd ts)
    TSArray t    -> tsDeps t
    TSMap k v    -> list [k,v]
  where
  list = Set.unions . map tsDeps

--------------------------------------------------------------------------------
instance PP name => PP (TSDecl name) where
  pp d =
    ("type" <+> pp (tsName d) <.> ps <+> "=") $$ nest 2 (pp (tsDef d))
    where ps = case tsParams d of
                 [] -> mempty
                 ts -> hcat [ "<", commaSep (map pp ts), ">" ]

instance PP name => PP (TSType name) where
  pp ty =
    case ty of
      TSBuiltIn txt -> pp txt
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



--------------------------------------------------------------------------------



data TypeDef = TypeDef
  { typeTS        :: TSType Name
  , typeToJSON    :: Q (Pat, Exp)           -- a -> Value
  , typeFromJSON  :: Q Exp -> Q Exp         -- Value -> Parser a
  , typeToJSKey   :: Maybe (Q (Pat, Exp))   -- a -> Text
  , typeFromJSKey :: Maybe (Q Exp -> Q Exp) -- Text -> Parser a
  }


nameLabel :: Name -> Text
nameLabel (Name (OccName s) _) = Text.pack s

typeToTS :: Type -> Either String (TSType Name)
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
    | otherwise         = pure (TSNamed c args)

  doApp f args =
    case f of
      AppT g x       -> doAppT g x args
      AppKindT g _   -> doApp g args
      SigT g _       -> doApp g args
      ParensT g      -> doApp g args

      VarT a
        | null args     -> pure (TSBuiltIn (nameLabel a))

      ConT c         -> doCon c args
      TupleT _       -> pure (TSTuple args)
      ListT
        | [t] <- args   -> pure (TSArray t)

      _                 -> unsupported


class TextToKey a where
  toTextKey   :: a -> Text
  fromTextKey :: Text -> JS.Parser a

instance TextToKey Text where
  toTextKey = id
  fromTextKey = pure

showToTextKey :: Show a => a -> Text
showToTextKey = Text.pack . show

readFromTextKey :: Read a => Text -> JS.Parser a
readFromTextKey x = case readMaybe (Text.unpack x) of
                      Just a  -> pure a
                      Nothing -> fail "Failed to read key"

instance TextToKey Bool where
  toTextKey = showToTextKey
  fromTextKey = readFromTextKey

instance TextToKey Int where
  toTextKey = showToTextKey
  fromTextKey = readFromTextKey

instance TextToKey Integer where
  toTextKey = showToTextKey
  fromTextKey = readFromTextKey




exportKeyType :: Name -> Q [Dec]
exportKeyType = exportType' True

exportType :: Name -> Q [Dec]
exportType = exportType' False

exportType' :: Bool -> Name -> Q [Dec]
exportType' flat x =
  do nd <- reifyDatatype x
     declToTS flat nd


declToTS :: Bool -> DatatypeInfo -> Q [Dec]
declToTS asKey nd =
  case mapM (doConDef untagged) cons of
    Left err -> fail err
    Right defs ->
      do ts <- toTS (map typeTS defs)
         tJ <- toJS (map typeToJSON defs)
         fJ <- fromJS (map typeFromJSON defs)
         let keys =
              do tos   <- mapM typeToJSKey defs
                 froms <- mapM typeFromJSKey defs
                 pure (mkJSKey tos froms)
         ks <- case keys of
                 Nothing | asKey -> fail "Cannot use type as key"
                         | otherwise -> pure []
                 Just kds -> sequence kds
         pure (tJ : fJ : ks ++ ts)
  where
  cons     = datatypeCons nd
  untagged = asKey
          || case datatypeCons nd of
               [_]  -> True
               _    -> all (null . constructorFields) cons

  toTS :: [TSType Name] -> Q [Dec]
  toTS ts =
    do dS <- sigD name [t| String |]
       let txt = "" :: String -- show (pp d)
       dE <- mkD name [e| txt |]
       pure [dS,dE]
    where
    name = mkName ("tys" ++ nameBase (datatypeName nd))
    d = TSDecl
          { tsName   = datatypeName nd
          , tsParams = map (nameLabel . tvName) (datatypeVars nd)
          , tsDef =
              case ts of
                  [t] -> t
                  _     -> TSUnion ts
         }

  mkD x e   = valD (varP x) (normalB e) []
  mkM m     = m >>= \(p,e) -> match (pure p) (normalB (pure e)) []
  jn f g    = [| $f <|> $g |]

  alts :: Q Exp -> [Q Exp -> Q Exp] -> Q Exp
  alts x xs = foldr1 jn [ f x | f <- xs ]

  toJS :: [Q (Pat,Exp)] -> Q Dec
  toJS ms =
    instanceD (pure ctx) (pure (jsC (datatypeType nd)))
      [ mkD 'JS.toJSON [| \x -> $(caseE [| x |] (map mkM ms)) |] ]
    where
    jsC t = classPred ''JS.ToJSON [t]
    ctx   = [ jsC (VarT (tvName a)) | a <- datatypeVars nd ]

  fromJS :: [Q Exp -> Q Exp] -> Q Dec
  fromJS xs =
    instanceD (pure ctx) (pure (jsC (datatypeType nd)))
      [ mkD 'JS.parseJSON [| \x -> $(alts [| x |] xs) |] ]
    where
    jsC t     = classPred ''JS.FromJSON [t]
    ctx       = [ jsC (VarT (tvName a)) | a <- datatypeVars nd ]

  mkJSKey :: [Q (Pat,Exp)] -> [Q Exp -> Q Exp] -> [Q Dec]
  mkJSKey tos froms =
    [ instanceD (pure ctx) (pure (jsC ty))
        [ mkD 'toTextKey   [| \x -> $(caseE [| x |] (map mkM tos)) |]
        , mkD 'fromTextKey [| \x -> $(alts [| x |] froms) |]
        ]

    , instanceD (pure ctx) (pure (classPred ''JS.ToJSONKey [ty]))
        [ mkD 'JS.toJSONKey [| JS.toJSONKeyText toTextKey |] ]

    , instanceD (pure ctx) (pure (classPred ''JS.FromJSONKey [ty]))
        [ mkD 'JS.fromJSONKey [| JS.FromJSONKeyTextParser fromTextKey |] ]
    ]
    where
    ty    = datatypeType nd
    jsC t = classPred ''TextToKey [t]
    ctx   = [ jsC (VarT (tvName a)) | a <- datatypeVars nd ]





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

        , typeToJSKey = Nothing
        , typeFromJSKey = Nothing
      }

  makeDef ts =
    case ts of

      -- No fields
      []

        -- Just text
        | untagged ->
            TypeDef
              { typeTS = TSLit conTag

              , typeToJSON =
                 do (p,_) <- lhsPat
                    e     <- [| JS.String conTag |]
                    pure (p, e)

              , typeToJSKey = Just
                  do (p,_) <- lhsPat
                     e     <- [| conTag |]
                     pure (p,e)

              , typeFromJSON = \j ->
                 [| JS.withText (Text.unpack conTag) (\txt ->
                      do guard (txt == conTag)
                         pure $conFun)
                    $j
                  |]

              , typeFromJSKey = Just \j ->
                 [| do guard ($j == conTag)
                       pure $conFun |]
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
                , typeToJSKey = Nothing
                , typeFromJSKey = Nothing
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

          , typeToJSKey = Just
            do (p,xs) <- lhsPat
               e <- [| toTextKey $(head xs) |]
               pure (p,e)

          , typeFromJSON = \j -> [| $conFun <$> $(qParseJson j) |]

          , typeFromJSKey = Just \j -> [| $conFun <$> fromTextKey $j |]
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
                           getF l x =
                             bindS (varP x) [| $obj JS..: JS.fromText l |]

                     [| JS.withObject "obj" $fromObj $j |]

              , typeToJSKey = Nothing
              , typeFromJSKey = Nothing
              }

            where
            labs = map nameLabel fs


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

              , typeToJSKey = Nothing
              , typeFromJSKey = Nothing
              }

  fields    = constructorFields con
  conTag    = nameLabel (constructorName con)
  conFun    = conE (constructorName con)
  conFunApp = foldl appE conFun . map varE
  fieldVars = mapM (const (newName "x")) fields
  lhsPat =
    do xs <- fieldVars
       p  <- conP (constructorName con) (map varP xs)
       pure (p, map varE xs)

  qToJson    = appE [| JS.toJSON |]
  qParseJson = appE [| JS.parseJSON |]


