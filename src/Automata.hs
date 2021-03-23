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

data AutomataState = AState { name           :: T.Text
                            , initial        :: Bool
                            , final          :: Bool
                            , adjacentStates :: Map.Map T.Text T.Text
                            }
  deriving stock (Eq, Show)

newtype AFD = AFD (Map.Map T.Text AutomataState)

next :: AFD -> AutomataState -> T.Text -> Either T.Text AutomataState
next (AFD m) st c = case adjacentNames of
                     Nothing -> Left $ "Failed by indeterminate transiction at state " <> name st
                     Just s -> let nextState = Map.lookup s m in
                       maybe (Left "This really shouldn't happen, check the Automata") Right nextState
  where
    adjacentNames = Map.lookup c (adjacentStates st)
