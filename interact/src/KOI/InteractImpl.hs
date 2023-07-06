module KOI.InteractImpl
  ( -- * InteractionState
    InteractState
  , startGame
  , GameInfo(..)
  , handleMessage
  , PlayerRequest
  , OutMsg

  -- * Building Interactions
  , Interact
  , askInputs
  , askInputsMaybe
  , askInputsMaybe_
  , choose
  , chooseMaybe
  , update
  , localUpdate
  , localUpdate_
  , getState
  , getsState

  , the
  , setThe
  , updateThe
  , updateThe_

  , save
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(foldl')
import Control.Monad(liftM,ap)
import GHC.Generics(Generic)
import Optics

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:?))
import qualified Data.Aeson as JS

import KOI.Basics
import KOI.AppTypes

startGame ::
  Component c => GameInfo c -> [WithPlayer (AppInput c)] -> InteractState c
startGame ginfo = foldl' step state0
  where
  state0 = interaction_ (Left (gInit ginfo))
           InteractState { iInit    = ginfo
                         , iLog     = []
                         , iName    = 0
                         , iGame    = gState ginfo
                         , iAsk     = Map.empty
                         , iQuestion = ""
                         , iShouldSave = False
                         , iC = gC ginfo
                         }
  step state i = interaction_ (Right i) state

data GameInfo c = GameInfo
  { gC       :: c
  , gPlayers :: Set PlayerId
  , gState   :: AppState c
  , gInit    :: Interact c ()
  , gSave    :: [WithPlayer (AppInput c)] -> String
  }


data OutMsg c =
    CurGameState (CurState c)
  | SetQuestion Text
  | AskQuestions (Text, [ChoiceHelp c])
  | GameUpdate (AppUpdateView c)
    deriving Generic

data ChoiceHelp c = ChoiceHelp
  { chChoice    :: AppInput c
  , chHelp      :: Text
  , chStateName :: Int    -- ties question to state
  } deriving Generic

data PlayerRequest c =
    Reload
  | Undo
  | PlayerResponse (ChoiceHelp c)


handleMessage ::
  Component c =>
  WithPlayer (PlayerRequest c) ->
  InteractState c ->
  (InteractState c, [WithPlayer (OutMsg c)], Maybe (Int,String))
handleMessage (p :-> req) =
  case req of
    Reload -> \s -> (s, [reload s p], Nothing)

    Undo -> \s ->
      case iLog s of
        (q :-> _) : more | p == q ->
           let s1 = startGame (iInit s) (reverse more)
           in (s1, map (reload s1) (iPlayers s1), Nothing)
        _ -> (s,[],Nothing)

    PlayerResponse ch ->
      \s -> if chStateName ch == iName s
              then askQuestions (interaction (Right (p :-> chChoice ch)) s)
              else (s,[],Nothing)


  where
  askQuestions (s,os) =
    ( s { iShouldSave = False }
    , reverse os ++
      [ q :-> SetQuestion (iQuestion s) | q <- iPlayers s ] ++
      [ q :-> AskQuestions (iQuestion s, qs)
      | (q,qs) <- Map.toList
                $ Map.fromListWith (flip (++))
                  [ (q,[ChoiceHelp { chChoice = ch
                                   , chHelp = help
                                   , chStateName = iName s
                                   }])
                      | (q :-> ch,(help,_))  <- Map.toList (iAsk s) ]
      ]
    , if iShouldSave s
         then Just (length (iLog s), gSave (iInit s) (reverse (iLog s)))
         else Nothing
    )



reload :: Component c => InteractState c -> PlayerId -> WithPlayer (OutMsg c)
reload s p =
  p :-> CurGameState
        CurState { cGame = playerView (iC s) p (iGame s)
                 , cQuestions =
                      ( iQuestion s
                      , [ ChoiceHelp { chChoice = q
                                     , chHelp = help
                                     , chStateName = iName s
                                     }
                        | (p' :-> q,(help,_)) <- Map.toList (iAsk s)
                        , p' == p ]
                      )
                 }



data InteractState c =
  InteractState
    { iInit   :: GameInfo c
      -- ^ Information about how the game was initialized.

    , iLog    :: [WithPlayer (AppInput c)]
      -- ^ A record of all responses made by the players

    , iName   :: !Int
      -- ^ An identifier for the state.  Used to check that the anser match
      -- the question.

    , iGame   :: AppState c
      -- ^ The current game state.
      -- Should be reproducable by replyaing the log file on the initial state

    , iAsk     :: Map (WithPlayer (AppInput c)) (Text, R c)
      -- ^ Choices avialable to the players.

    , iQuestion :: Text
      -- ^ A desicription of what we are doing.

    , iShouldSave :: Bool
      -- ^ Indicates if this is a save point.

    , iC :: !c
    }

iPlayers :: InteractState c -> [PlayerId]
iPlayers = Set.toList . gPlayers . iInit


-- | A monad for writing interactive turn based games.
newtype Interact c a = Interact ((a -> R c) -> R c)

instance Functor (Interact c) where
  fmap = liftM

instance Applicative (Interact c) where
  pure a = Interact \k -> k a
  (<*>)  = ap

instance Monad (Interact c) where
  Interact m >>= f = Interact \k -> m \a -> let Interact m1 = f a
                                            in m1 k

type R c = InteractState c -> [AppUpdate c] -> (InteractState c, [AppUpdate c])

-- | Perform an interaction
interaction ::
  Component c =>
  Either (Interact c ()) (WithPlayer (AppInput c)) ->
  InteractState c -> (InteractState c, [WithPlayer (OutMsg c)])
interaction how s = (s1,msgs)
  where
  Interact m =
     case how of
       Left m'    -> m'
       Right resp -> continueWith resp

  (s1,os) = m (const (,)) s []
  msgs    = [ p :-> o | p <- iPlayers s1
                      , o <- map (GameUpdate . playerUpdateView (iC s) p) os ]

interaction_ ::
  Component c =>
  Either (Interact c ()) (WithPlayer (AppInput c)) ->
  InteractState c -> InteractState c
interaction_ how = fst . interaction how

-- | Choose one of the given options.
-- The list of possible answers should be non-empty.
choose ::
  Component c =>
  PlayerId       {- ^ Ask this player -} ->
  Text           {- ^ Description of what we are asking the player -} ->
  [(AppInput c,Text)] {- ^ List of possible answers, with description -} ->
  Interact c (AppInput c) {- ^ Chosen answer -}
choose playerId q opts =
  askInputs q [ (playerId :-> ch, help, pure ch) | (ch,help) <- opts ]

-- | Ask a question but only if there is a choice.
-- Returns `Nothing` if there are no valid answers.
-- If there is only a single valid answer,
-- then select it without asking the player.
chooseMaybe ::
  Component c =>
  PlayerId               {- ^ Ask this player -} ->
  Text                   {- ^ Description of what we are asking the player -} ->
  [(AppInput c,Text)]         {- ^ List of possible answers, with description -} ->
  Interact c (Maybe (AppInput c)) {- ^ Chosen answer, if any -}
chooseMaybe playerId q opts =
  case opts of
    []  -> pure Nothing
    [t] -> pure (Just (fst t))
    _   -> Just <$> choose playerId q opts

-- | Ask one or more players a question and continue based on their answer.
askInputs ::
  Component c =>
  Text {- ^ Desciption of what we are asking -} ->
  [ (WithPlayer (AppInput c), Text, Interact c a) ]
  {- ^ Possible answers for various players.

       * The first component specifies the player and what they see.
       * The second is a description of the choice.
       * The third is how to continue if this answer is selected. -} ->
  Interact c a
askInputs q opts =
  Interact $
  \curK ->
  \curS os ->
  let cont (ch,help,Interact m) = (ch, (help, m curK))
  in (curS { iQuestion = q
           , iAsk = Map.union (Map.fromList (map cont opts)) (iAsk curS) }, os)

{- | Ask one or more players a question and continue based on their answer.

  * Returns 'Nothing' if there are no valid answers.
  * If there is only one possible answer, then it is automatically selected
    without asking the player.
-}
askInputsMaybe ::
  Component c =>
  Text {- ^ Desciption of what we are asking -} ->
  [ (WithPlayer (AppInput c), Text, Interact c a) ]
  {- ^ Possible answers for various players.

       * The first component specifies the player and what they see.
       * The second is a description of the choice.
       * The third is how to continue if this answer is selected. -} ->
  Interact c (Maybe a)
askInputsMaybe txt opts =
  case opts of
    [] -> pure Nothing
    [(_,_,m)] -> Just <$> m
    _ -> Just <$> askInputs txt opts


{- | Ask one or more players a question and continue based on their answer.

  * Equivalent to `pure ()` if there are no valid choices.
  * If there is only one possible answer, then it is automatically selected
    without asking the player.
-}
askInputsMaybe_ ::
  Component c =>
  Text {- ^ Desciption of what we are asking -} ->
  [ (WithPlayer (AppInput c), Text, Interact c ()) ]
  {- ^ Possible answers for various players.

       * The first component specifies the player and what they see.
       * The second is a description of the choice.
       * The third is how to continue if this answer is selected. -} ->
  Interact c ()
askInputsMaybe_ txt opts =
  case opts of
    []        -> pure ()
    [(_,_,m)] -> m
    _         -> askInputs txt opts




-- | Resume execution based on player input
continueWith :: Component c => WithPlayer (AppInput c) -> Interact c a
continueWith msg = Interact $
  \_ ->
  \s os ->
    case Map.lookup msg (iAsk s) of
      Just (_,f)  ->
        let s1 = s { iAsk = Map.empty
                   , iLog = msg : iLog s, iName = iName s + 1 }
        in f s1 os
      Nothing -> (s,os)

getsState :: Component c => (AppState c -> a) -> Interact c a
getsState f = Interact $
  \k ->
  \s os -> case iGame s of
             st | finalState (iC s) st -> (s,os)
                | otherwise -> k (f st) s os

-- | Get the value of the given field of the state
the :: Component c => Lens' (AppState c) a -> Interact c a
the f = getsState (view f)

-- | Access the current game state
getState :: Component c => Interact c (AppState c)
getState = getsState id

-- | Update the current game state
update :: Component c => AppUpdate c -> Interact c ()
update o = Interact $
  \k    ->
  \s os -> case iGame s of
             a | finalState (iC s) a -> (s,os)
               | otherwise    -> k () s { iGame = doUpdate (iC s) o a } (o : os)

-- | Updates that are not visible to the players
localUpdate :: Component c => (AppState c -> (a,AppState c)) -> Interact c a
localUpdate f = Interact $
  \k ->
  \s os -> case iGame s of
             a | finalState (iC s) a -> (s,os)
               | otherwise    -> case f a of
                                   (x,b) -> k x s { iGame = b } os

-- | Updates that are not visible to the players
localUpdate_ :: Component c => (AppState c -> AppState c) -> Interact c ()
localUpdate_ f = localUpdate \s -> ((),f s)

setThe :: Component c => Lens' (AppState c) a -> a -> Interact c ()
setThe x a = localUpdate_ (set x a)

updateThe :: Component c => Lens' (AppState c) a -> (a -> (b,a)) -> Interact c b
updateThe x f = localUpdate \s -> case f (view x s) of
                                    (b,x1) -> (b, set x x1 s)

updateThe_ :: Component c => Lens' (AppState c) a -> (a -> a) -> Interact c ()
updateThe_ x f = localUpdate_ (over x f)


-- | Save the current game state.
save :: Interact c ()
save = Interact $ \k s os -> k () s { iShouldSave = True } os


--------------------------------------------------------------------------------
-- Input and output messages

data CurState c = CurState
  { cGame       :: AppStateView c
  , cQuestions  :: (Text, [ChoiceHelp c])
  }


instance Component c => ToJSON (OutMsg c)

instance Component c => ToJSON (CurState c) where
  toJSON g = JS.object
    [ "game"      .= cGame g
    , "questions" .= cQuestions g
    ]

instance Component c => FromJSON (PlayerRequest c) where
  parseJSON =
    JS.withObject "request" \o ->
    do tag <- o .:? "tag"
       case tag :: Maybe Text of
         Just "reload" -> pure Reload
         Just "undo"   -> pure Undo
         _ -> PlayerResponse <$> parseJSON (JS.Object o)


instance Component c => ToJSON (ChoiceHelp c)
instance Component c => FromJSON (ChoiceHelp c)

--------------------------------------------------------------------------------

