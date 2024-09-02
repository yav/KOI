-- | A server to play a game.
module KOI.Interact
  ( -- * Starting a server
    startApp
  , App(..)
  , Component(..)

    -- * Interaction
  , Interact

    -- ** User input
  , askInputs
  , askInputsMaybe
  , askInputsMaybe_
  , choose
  , chooseMaybe

    -- ** Read the state
  , getState
  , getsState

    -- ** Modify the state
  , update
  , localUpdate
  , localUpdate_


    -- ** Field based access to the state
  , the
  , setThe
  , updateThe
  , updateThe_

    -- ** Save the game
  , Interact.save
  ) where

import Data.Text(Text)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set

import KOI.Basics
import KOI.CallJS(jsHandlers)
import KOI.RNG
import KOI.Options

import KOI.Server
import KOI.InteractImpl hiding (save)
import qualified KOI.InteractImpl as Interact

import KOI.AppTypes

-- | Desciption of an application.
data App c = App
  { appId :: c
    -- ^ Identifier for the app

  , appOptions      :: [Option]
    -- ^ Command line flags

  , appColors       :: [Text]
    -- ^ Available player colors

  , appJS           :: String
    -- ^ Javascript to be exported via dynamic.js

  , appInitialState ::
      RNG -> Options -> [PlayerId] -> IO (AppState c)
    -- ^ Initial game state

  , appStart        :: Interact c ()
    -- ^ Execute this to start the game
  }

data Component c => Save c = Save
  { seed  :: Seed
  , moves :: [WithPlayer (AppInput c)]
  , opts  :: Options
  } deriving (Read,Show)


-- | Start a web server for the given application.
startApp :: Component c => App c -> IO ()
startApp app =
  newServer (optionsGetOpt (appOptions app)) \opts' ->
    do let fullOpts = opts' <> optionsDefaults (appOptions app)
       save <- case getOptLoad fullOpts of
                 Just file -> read <$> readFile file
                 Nothing   -> do theSeed <- newRNGSeed
                                 pure Save { seed = theSeed
                                           , moves = []
                                           , opts = fullOpts }
       begin app save
       
begin :: Component c => App c -> Save c -> IO (ByteString, InteractState c)
begin app save =
  do state <- appInitialState app rng (opts save) ps
     let outMsg = $(jsHandlers [''OutMsg])
     pure ( BS8.unlines [ cols, outMsg, BS8.pack (appJS app) ]
          , startGame GameInfo
                  { gPlayers = Set.fromList ps
                  , gState   = state
                  , gInit    = appStart app
                  , gSave    = \m -> show (save { moves = m } `asTypeOf` save)
                  , gC       = appId app
                  }
                  (moves save)
          )
 where
 (ps,cols)  = getOptPlayers (appColors app) (opts save)
 rng        = seedRNG (seed save)
