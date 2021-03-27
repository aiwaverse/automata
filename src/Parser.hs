module Parser (unitParser, functionProgramParser, afdParser) where

import Text.Megaparsec
import Text.Megaparsec.Char ( string, alphaNumChar, char ) 
import Data.Text (Text, pack)
import Data.Void

import Automata

type Parser = Parsec Void Text

unitParser :: Parser [Text]
unitParser = do
  unit <- some alphaNumChar `sepBy` char ','
  return . map pack $ unit

functionProgramParser :: Parser (Text, Text, Text)
functionProgramParser = do
  _ <- char '('
  stateA <- some alphaNumChar
  _ <- char ','
  symbol <- some alphaNumChar 
  _ <- string ")="
  stateB <- some alphaNumChar 
  return (pack stateA, pack symbol, pack stateB)

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
