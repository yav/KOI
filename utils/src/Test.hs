module Test where

import KOI.DeriveTS
import Data.Map qualified as Map
import Data.Aeson qualified as JS

data X = A | B
  deriving (Eq,Ord,Show)

data Y = C | D X
  deriving (Eq,Ord,Show)

$(exportKeyType ''X)
$(exportKeyType ''Y)


ex1 = Map.fromList [ (D A, 1 :: Int), (C,2) ]
