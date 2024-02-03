module App.StateView where

import KOI.Basics
import Data.Aeson qualified as JS
import App.State

newtype StateView = StateView State

getStateView :: PlayerId -> State -> StateView
getStateView _pid = StateView

instance JS.ToJSON StateView where
  toJSON (StateView (State _ n)) = JS.toJSON n
