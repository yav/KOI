module Main where

import KOI.CallJS(jsHandlers)
import App.KOI
import App.State
import App.Input

main :: IO ()
main = startApp App
  { appId = KOI
  , appOptions = []
  , appColors = [ "red", "green", "blue", "yellow" ]
  , appJS = $(jsHandlers [ ''Update, ''Input ])
  , appInitialState = \_rng _opts ps ->
      case ps of
        [p] -> Right (State p 0)
        _   -> Left "We need exactly 1 playerm, see --help"
  , appStart = gameLoop
  }

gameLoop :: Interact ()
gameLoop =
  do State p n <- getState
     what <- choose p "What should we do next"
                [ (Inc, "Increment"), (Dec, "Decrement") ]
     update $ SetState
            $ State p
              case what of
                Inc -> n + 1
                Dec -> n - 1
     gameLoop
