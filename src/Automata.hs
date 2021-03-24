-- |
-- Copyright: (c) 2021 phi
-- SPDX-License-Identifier: MIT
-- Maintainer: phi <aiwaverse@protonmail.com>
--
-- See README for more info
{-# LANGUAGE DerivingStrategies #-}
module Automata
  ( AutomataState(..)
  , AFD(..)
  , next
  , getInitial
  , validate
  , projectName
  ) where

import qualified Data.Map  as Map

import qualified Data.Text as T


projectName :: String
projectName = "automata"

-- | The type that describes an automata state
data AutomataState = AState { name           :: T.Text
                            , initial        :: Bool
                            , final          :: Bool
                            , adjacentStates :: Map.Map Char T.Text
                            }
  deriving stock (Eq, Show)

-- | an AFD is nothing more than a set of labels and states
newtype AFD = AFD (Map.Map T.Text AutomataState)

-- | Given the automata, the current state @st and a text @c, returns the next state, if available
next :: AFD -> AutomataState -> Char -> Either T.Text AutomataState
next (AFD m) st c = case adjacentNames of
  Nothing -> Left $ "Failed by indeterminate transiction at state " <> name st
  Just s ->
    let nextState = Map.lookup s m
    in  maybe
          (  Left
          $  "A state name was referenced but it's not in the Automata,"
          <> " this really shouldn't happen, check the Automata."
          )
          Right
          nextState
  where adjacentNames = Map.lookup c (adjacentStates st)

getInitial :: AFD -> AutomataState
getInitial (AFD m) = case Map.toList $ Map.filter initial m of
  []  -> error "No initial state on the Automata"
  [x] -> snd x
  _   -> error "More than one initial state on the Automata"

validate :: AFD -> T.Text -> Either T.Text Bool
validate automata w = case go initialState w of
  Right b -> if b then Right b else Left "Rejected by not reacing a final state"
  Left  t -> Left t
 where
  initialState = getInitial automata
  go :: AutomataState -> T.Text -> Either T.Text Bool
  go currState (T.null -> True) = Right $ final currState
  go currState wordToTest       = case next automata currState (T.head wordToTest) of
    Right s -> go s (T.tail wordToTest)
    Left  s -> Left s
