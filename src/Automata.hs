-- |
-- Copyright: (c) 2021 phi
-- SPDX-License-Identifier: MIT
-- Maintainer: phi <aiwaverse@protonmail.com>
--
-- See README for more info
{-# LANGUAGE DerivingStrategies #-}
module Automata
  ( AutomataState (..)
  , AFD (..)
  , next
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
                            , adjacentStates :: Map.Map T.Text T.Text
                            }
  deriving stock (Eq, Show)

-- | an AFD is nothing more than a set of labels and states
newtype AFD = AFD (Map.Map T.Text AutomataState)

-- | Given the automata, the current state @st and a text @c, returns the next state, if available
next :: AFD -> AutomataState -> T.Text -> Either T.Text AutomataState
next (AFD m) st c = case adjacentNames of
                     Nothing -> Left $ "Failed by indeterminate transiction at state " <> name st
                     Just s -> let nextState = Map.lookup s m in
                       maybe (Left "This really shouldn't happen, check the Automata") Right nextState
  where
    adjacentNames = Map.lookup c (adjacentStates st)
