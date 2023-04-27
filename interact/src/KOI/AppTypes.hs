-- | To use the interaction framework, games need to implement this signature.
module KOI.AppTypes where

import Data.Aeson(ToJSON,FromJSON)
import KOI.Basics(PlayerId)

class
  ( ToJSON (AppInput c)
  , ToJSON (AppStateView c)
  , ToJSON (AppUpdateView c)

  , FromJSON (AppInput c)
  , Ord (AppInput c)
  , Show (AppInput c)
  , Read (AppInput c)
  ) =>
  Component c where

  -- | The state of a game in progress.
  type family AppState c

  -- | A view of the current game state that can be sent to a player.
  type family AppStateView c

  -- | Possible updates to a game's state.
  type family AppUpdate c

  -- | A view of an update that can be sent to a player.
  type family AppUpdateView c

  -- | Possible player inputs.
  type family AppInput c

  -- | Apply an update to a game in progress.
  doUpdate         :: c -> AppUpdate c -> AppState c -> AppState c

  -- | Does this state represent a finished game
  finalState       :: c -> AppState c -> Bool

  -- | The state of the game as seen by the given player.
  playerView       :: c -> PlayerId -> AppState c -> AppStateView c

  -- | An update to game state as seen by the given player.
  playerUpdateView :: c -> PlayerId -> AppUpdate c -> AppUpdateView c


