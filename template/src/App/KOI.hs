module App.KOI
  ( KOI(..)
  , Update(..)
  , Interact
  , module KOI.Interact
  ) where

import KOI.Interact hiding (Interact)
import KOI.Interact qualified as I

import App.State(State, stateIsFinal)
import App.StateView(StateView, getStateView)
import App.Input(Input)

data KOI = KOI

type Interact = I.Interact KOI

instance Component KOI where
  type AppState       KOI = State
  type AppStateView   KOI = StateView
  type AppUpdate      KOI = Update
  type AppUpdateView  KOI = StateView
  type AppInput       KOI = Input

  doUpdate _ (SetState s) _         = s
  playerView _                      = getStateView
  playerUpdateView c p (SetState s) = playerView c p s
  finalState _                      = stateIsFinal

data Update = SetState State
