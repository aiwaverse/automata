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
  , projectName
  ) where

import qualified Data.Map  as Map

import qualified Data.Text as T

import Data.List(foldl')

projectName :: String
projectName = "automata"

-- | The type that describes an automata state
data AutomataState = AState { name           :: T.Text
                            , initial        :: Bool
                            , final          :: Bool
                            , adjacentStates :: Map.Map T.Text T.Text
                            }
  deriving stock (Eq, Show)

-- | an AFD is nothing more than a set of labels and states
newtype AFD = AFD (Map.Map T.Text AutomataState)

-- | Given the automata, the current state @st and a text @c, returns the next state, if available
next :: AFD -> AutomataState -> T.Text -> Either T.Text AutomataState
next (AFD m) st c = case adjacentNames of
  Nothing -> Left $ "Failed by indeterminate transiction at state " <> name st
  Just s ->
    let nextState = Map.lookup s m
    in  maybe (Left "This really shouldn't happen, check the Automata") Right nextState
  where adjacentNames = Map.lookup c (adjacentStates st)

getInitial :: AFD -> Maybe AutomataState
getInitial (AFD m) = case Map.toList $ Map.filter initial m of
  []  -> Nothing
  [x] -> Just $ snd x
  _   -> Nothing

validate :: AFD -> T.Text -> Either T.Text Bool
validate automata w = undefined
  where
    initialState = getInitial automata
    go :: AutomataState -> T.Text -> Either T.Text Bool
    go currState (T.null -> True) = Right $ final currState
    go currState wordToTest = case next automata currState (T.pack [T.head wordToTest]) of
                                  Left t -> Left t
                                  Right s -> go s (T.tail wordToTest)