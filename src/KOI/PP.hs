module KOI.PP
  ( Doc
  , toText
  , PP(..)
  , (<+>), (<.>), ($$)
  , hcat, fcat, vcat
  , hsep, fsep
  , brackets, parens, braces
  , nest
  , commaSep
  ) where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.String
import Text.PrettyPrint qualified as P

newtype Doc = Doc P.Doc

instance Show Doc where
  show (Doc s) = show s

instance IsString Doc where
  fromString = lift0 . P.text

instance Semigroup Doc where
  (<>) = (<.>)

instance Monoid Doc where
  mempty = lift0 P.empty

toText :: Doc -> Text
toText = Text.pack . show

--------------------------------------------------------------------------------

class PP a where
  pp :: a -> Doc
  ppList :: [a] -> Doc
  ppList = fsep . map pp

instance PP Doc where
  pp = id

instance PP a => PP [a] where
  pp = ppList
  ppList xs = vcat [ text "*" <+> pp x | x <- xs ]

instance PP Char where
  pp     = lift0 . P.char
  ppList = lift0 . P.text

instance PP Text where
  pp     = lift0 . P.text . Text.unpack

instance PP Int where
  pp     = lift0 . P.int

instance PP Integer where
  pp     = lift0 . P.integer



--------------------------------------------------------------------------------
text :: Text -> Doc
text = pp

infixl 6 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) = lift2 (P.<+>)

infixl 6 <.>
(<.>) :: Doc -> Doc -> Doc
(<.>) = lift2 (P.<>)

infixl 5 $$
($$) :: Doc -> Doc -> Doc
($$) = lift2 (P.$$)

hsep :: [Doc] -> Doc
hsep = liftMany (P.hsep)

fsep :: [Doc] -> Doc
fsep = liftMany (P.fsep)

hcat :: [Doc] -> Doc
hcat = liftMany (P.hcat)

fcat :: [Doc] -> Doc
fcat = liftMany (P.fcat)

vcat :: [Doc] -> Doc
vcat = liftMany (P.vcat)

brackets :: Doc -> Doc
brackets = lift1 P.brackets

parens :: Doc -> Doc
parens = lift1 P.parens

braces :: Doc -> Doc
braces = lift1 P.braces

nest :: Int -> Doc -> Doc
nest n = lift1 (P.nest n)

commaSep :: [Doc] -> Doc
commaSep = liftMany (P.hcat . P.punctuate P.comma)

--------------------------------------------------------------------------------

lift0 :: P.Doc -> Doc
lift0 = Doc

lift1 :: (PP a) => (P.Doc -> P.Doc) -> a -> Doc
lift1 f = \(pp -> Doc x) -> lift0 (f x)

lift2 :: (PP a, PP b) => (P.Doc -> P.Doc -> P.Doc) -> a -> b -> Doc
lift2 f = \(pp -> Doc x) (pp -> Doc y) -> lift0 (f x y)

liftMany :: PP a => ([P.Doc] -> P.Doc) -> [a] -> Doc
liftMany f = \xs -> lift0 (f [ x | (pp -> Doc x) <- xs])

