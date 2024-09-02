-- | A deck and a discard pile.
module KOI.ResourceQ
  ( ResourceQ
  , rqEmpty
  , rqFromListRandom
  , rqFromListOrdered
  , rqTake
  , rqTakeN
  , rqDiscard
  ) where

import KOI.RNGM

-- | A deck and a discard pile.  If the deck runs out, then we
-- shuffle the discard pile.
data ResourceQ a = ResourceQ
  { qAvailable   :: [a]
  , qDiscarded   :: [a]
  , qRandom      :: RNG
  }

-- | Empty deck and discard.
rqEmpty :: Gen (ResourceQ a)
rqEmpty =
  do qRandom <- splitRNG
     return ResourceQ { qAvailable = [], qDiscarded = [], .. }

-- | Empty deck, but the given things are in the discard.
rqFromListRandom :: [a] -> Gen (ResourceQ a)
rqFromListRandom qDiscarded =
  do qRandom <- splitRNG
     return ResourceQ { qAvailable = [], .. }

-- | Empty discard, set the deck to the given list.
rqFromListOrdered :: [a] -> Gen (ResourceQ a)
rqFromListOrdered qAvailable =
  do qRandom <- splitRNG
     return ResourceQ { qDiscarded = [], .. }

-- | Take something from the deck.  If the deck is empty,
-- shuffle the discard and use it as the new deck.
-- Returns 'Nothing' if both the deck and the discard are empty.
rqTake :: ResourceQ a -> Maybe (a, ResourceQ a)
rqTake ResourceQ { .. } =
  case qAvailable of
    a : as -> Just (a, ResourceQ { qAvailable = as, .. })
    [] -> case runRNG qRandom (shuffle qDiscarded) of
            (a:as,g) -> Just (a, ResourceQ { qAvailable = as
                                           , qDiscarded = []
                                           , qRandom    = g })
            ([],_) -> Nothing

-- | Try to take the given number of things. Shuffles if needed.
-- May return fewer if not enough present.
rqTakeN :: Int -> ResourceQ a -> ([a], ResourceQ a)
rqTakeN n q
  | n > 0, Just (a,new) <- rqTake q =
    let (as,q1) = rqTakeN (n-1) new
    in (a : as, q1)
  | otherwise = ([],q)

-- | Place something in the discard pile.
rqDiscard :: a -> ResourceQ a -> ResourceQ a
rqDiscard a ResourceQ { .. } = ResourceQ { qDiscarded = a : qDiscarded, .. }



