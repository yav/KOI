module App.State where

import KOI.Basics

data State = State PlayerId Int
  deriving (Read,Show)

stateIsFinal :: State -> Bool
stateIsFinal _ = False


