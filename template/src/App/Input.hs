module App.Input where

import Data.Aeson qualified as JS

data Input = Inc | Dec
  deriving (Read,Show,Eq,Ord)

instance JS.ToJSON Input where
  toJSON i =
    case i of
      Inc -> "Inc"
      Dec -> "Dec"

instance JS.FromJSON Input where
  parseJSON = JS.withText "Input"
    \case
      "Inc" -> pure Inc
      "Dec" -> pure Dec
      _     -> fail "Invalid"


