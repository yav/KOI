module App.Input where

import GHC.Generics(Generic)
import Data.Aeson qualified as JS

data Input = Inc | Dec
  deriving (Read,Show,Eq,Ord,Generic,JS.ToJSON,JS.FromJSON)



