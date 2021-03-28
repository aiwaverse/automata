module AFDCreator
  ( makeAFD
  , makeAFDStates
  , makeAState
  ) where


import           Automata
    (AFD (..), AutomataState (AState))
import           Data.Bifunctor
    (second)
import           Data.List
    (groupBy, nub, sort, sortOn)
import qualified Data.Map       as Map
import           Data.Maybe
    (fromMaybe)
import           Data.Text
    (Text)
import qualified Data.Text      as T
    (head)

makeAFD :: ([Text], Text, [Text], [(Text, Text, Text)]) -> AFD
makeAFD (aStates, iState, fStates, functionProgram) = AFD afdMap
 where
  afdMap = Map.fromList
    $ zip (sort aStates) (makeAFDStates (aStates, iState, fStates, functionProgram))

-- | Given the list of states @aStates, the initial state, the final states and the
-- functionProgram as parsed, return a list of states
makeAFDStates :: ([Text], Text, [Text], [(Text, Text, Text)]) -> [AutomataState]
makeAFDStates (aStates, iState, fStates, functionProgram) = map
  (\s -> partialState s (transictions s))
  automataStates
 where
  automataStates    = sort aStates
  builtTransictions = sortOn fst (groupFunction functionProgram)
  partialState st = makeAState (st, iState, fStates)
  transictions st = fromMaybe Map.empty (lookup st builtTransictions)

-- | Given @aState, the initial state and a list of final sates, returns an
-- AutomataState that waits for the adjacentStates
makeAState :: (Text, Text, [Text]) -> Map.Map Char Text -> AutomataState
makeAState (aState, iState, fStates) =
  AState aState (aState == iState) (aState `elem` fStates)

-- | Given a list of parsed program functions, returns a list containing the name of the states and their
-- transiction table
-- >>> groupFunction [("q0","a","q1"),("q0","b","q0"),("q1","a","q0"),("q1","b","q1")]
-- [("q0",fromList [('a',"q1"),('b',"q0")]),("q1",fromList [('a',"q0"),('b',"q1")])]
groupFunction :: [(Text, Text, Text)] -> [(Text, Map.Map Char Text)]
groupFunction fp = map (second Map.fromList) statesBuilt
 where
  tripleFst (a, _, _) = a
  sndThd (_, b, c) = (T.head b, c)
  grouped     = groupBy (\x y -> tripleFst x == tripleFst y) fp
  name        = nub $ map tripleFst fp
  states      = map (map sndThd) grouped
  statesBuilt = zip name states

