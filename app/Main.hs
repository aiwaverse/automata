module Main
  ( main
  ) where

import           AFDCreator
import           Automata
import           FileParser

import           Text.Megaparsec
    (parseMaybe)

import qualified Data.Text       as T
    (pack, unwords)
import qualified Data.Text.IO    as TIO

import           Data.Maybe
    (fromMaybe)

main :: IO ()
main = do
  TIO.putStrLn "Enter the string"
  input <- TIO.getLine
  let (aName, aStates, alphabet, iState, fStates, functionProgram) =
        fromMaybe ("", [], [], "", [], []) (parseMaybe afdFileParser input)
  let afd = makeAFD (aStates, iState, fStates, functionProgram)
  TIO.putStrLn $ "Automata " <> aName <> " on alphabet " <> T.unwords alphabet
  TIO.putStrLn $ T.pack . show $ afd
