module Main
  ( main
  ) where

import           Automata
import qualified Data.Map   as Map
import           Test.HUnit

stateQ0 :: AutomataState
stateQ0 = AState "q0" True True (Map.fromList [('a', "q1"), ('b', "q0")])
stateQ1 :: AutomataState
stateQ1 = AState "q1" False False (Map.fromList [('a', "q0"), ('b', "q1")])

automataT :: AFD
automataT = AFD $ Map.fromList [(name stateQ0, stateQ0), (name stateQ1, stateQ1)]

test1 :: Test
test1 = TestCase $ assertEqual "a at q0" (Right stateQ1) (next automataT stateQ0 'a')
test2 :: Test
test2 = TestCase $ assertEqual "b at q0" (Right stateQ0) (next automataT stateQ0 'b')
test3 :: Test
test3 = TestCase $ assertEqual "a at q1" (Right stateQ0) (next automataT stateQ1 'a')
test4 :: Test
test4 = TestCase $ assertEqual "b at q1" (Right stateQ1) (next automataT stateQ1 'b')
test5 :: Test
test5 = TestCase $ assertEqual "c at q0"
                               (Left "Failed by indeterminate transiction at state q0")
                               (next automataT stateQ0 'c')
test6 :: Test
test6 = TestCase $ assertEqual "q0 is initial" stateQ0 (getInitial automataT)
test7 :: Test
test7 = TestCase
  $ assertEqual "Empty string with automataT" (Right True) (validate automataT "")
test8 :: Test
test8 = TestCase $ assertEqual "String \"aaa\" with automataT"
                               (Left "Rejected by not reacing a final state")
                               (validate automataT "aaa")
test9 :: Test
test9 = TestCase $ assertEqual "String \"aa\" with automataT"
                               (Right True)
                               (validate automataT "aa")
                          

tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9]

main :: IO ()
main = runTestTTAndExit tests
