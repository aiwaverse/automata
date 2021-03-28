module Parser (unitParser, functionProgramParser, afdParser) where

import Text.Megaparsec
import Text.Megaparsec.Char ( string, alphaNumChar, char )
import Data.Text (Text, pack)
import Data.Void

import Automata

type Parser = Parsec Void Text

-- | Parses an "unit", which is a set of values separated by comma
unitParser :: Parser [Text]
unitParser = do
  unit <- some alphaNumChar `sepBy` char ','
  return . map pack $ unit

-- | The parser used for a function program, which has the form (qn,c)=qm
functionProgramParser :: Parser (Text, Text, Text)
functionProgramParser = do
  _ <- char '('
  stateA <- some alphaNumChar
  _ <- char ','
  symbol <- some alphaNumChar
  _ <- string ")="
  stateB <- some alphaNumChar
  return (pack stateA, pack symbol, pack stateB)

-- | The parser that will HOPEFULLY create an AFD, currently returns a very ugly tuple
-- Either the return type or the name will change, certainly
afdParser :: Parser (Text, [Text], [Text], Text, [Text], [(Text, Text, Text)])
afdParser = let unitBetweenCurly = between (char '{') (char '}') unitParser in
  do
  automataName <- some alphaNumChar
  _ <- string "=("
  automataStates <- unitBetweenCurly
  _ <- char ','
  alphabet <- unitBetweenCurly
  _ <- char ','
  programName <- some alphaNumChar
  initialState <- between (char ',') (char ',') (some alphaNumChar)
  finalStates <- unitBetweenCurly
  _ <- char ')'
  _ <- string (pack programName)
  programFunction <- many functionProgramParser
  return (pack automataName, automataStates, alphabet, pack initialState, finalStates, programFunction)
