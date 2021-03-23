module Main (main) where

import           Automata
import qualified Data.Map   as Map
import           Test.HUnit

stateQ0 = AState "q0" True True (Map.fromList [("a", "q1"), ("b", "q0")])
stateQ1 = AState "q1" False False (Map.fromList [("a", "q0"), ("b", "q1")])

automataT = AFD $ Map.fromList [(name stateQ0, stateQ0), (name stateQ1, stateQ1)]

test1 = TestCase $ assertEqual "a at q0"  (Right stateQ1) (next automataT stateQ0 "a")
test2 = TestCase $ assertEqual "b at q0" (Right stateQ0) (next automataT stateQ0 "b")
test3 = TestCase $ assertEqual "a at q1" (Right stateQ0) (next automataT stateQ1 "a")
test4 = TestCase $ assertEqual "b at q1" (Right stateQ1) (next automataT stateQ1 "b")
test5 = TestCase $ assertEqual "c at q0" 
                               (Left "Failed by indeterminate transiction at state q0") 
                               (next automataT stateQ0 "c")
tests = TestList [test1, test2, test3, test4, test5]

main :: IO ()
main = runTestTTAndExit tests
