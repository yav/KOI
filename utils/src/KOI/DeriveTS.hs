module KOI.DeriveTS where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map(Map)
import KOI.PP
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH qualified as TH

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


test :: [TH.Dec] -> TH.Q TH.Exp
test ds =
  do ts <- mapM declToTS ds
     TH.litE (TH.stringL (show (pp (vcat (map pp ts)))))

fromName :: TH.Name -> Text
fromName (TH.Name (TH.OccName s) _) = Text.pack s

typeToTS :: TH.Type -> TH.Q TSType
typeToTS ty = doApp ty []
  where
  unsupported = fail "Unsupported type."

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
      TH.AppT g x       -> doAppT g x args
      TH.AppKindT g _   -> doApp g args
      TH.SigT g _       -> doApp g args
      TH.ParensT g      -> doApp g args

      TH.VarT a
        | null args     -> pure (TSNamed (fromName a) [])

      TH.ConT c         -> doCon c args
      TH.TupleT _       -> pure (TSTuple args)
      TH.ListT
        | [t] <- args   -> pure (TSArray t)

      _                 -> unsupported


declToTS :: TH.Dec -> TH.Q TSDecl
declToTS d =
  do nd <- normalizeDec d
     def <-
       case datatypeCons nd of
         [con] -> doCon con
         cons  -> doUnionType cons
     pure TSDecl
      { tsName = fromName (datatypeName nd)
      , tsParams = map (fromName . tvName) (datatypeVars nd)
      , tsDef = def
      }
  where
  doUnionType cons
    | all (null . constructorFields) cons =
      pure (TSUnion (map (TSLit . fromName . constructorName) cons))
    | otherwise =
      do cs <- mapM doCon cons
         let mkCon c def = TSRecord
                            [ ("tag", TSLit (fromName (constructorName c)))
                            , ("contents", def )
                            ]
         pure (TSUnion (zipWith mkCon cons cs))

  doCon con =
    do ts <- mapM typeToTS (constructorFields con)
       pure
         case constructorVariant con of
           RecordConstructor fs ->
             TSRecord (zip (map fromName fs) ts)
           _ -> TSTuple ts





