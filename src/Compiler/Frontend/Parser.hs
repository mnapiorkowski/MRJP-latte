module Frontend.Parser where

import System.IO
import System.Exit  (exitFailure)

import Latte.Abs
import Latte.Par    (pProgram, myLexer)

import Common

parse :: String -> IO Program
parse file = do
  case pProgram (myLexer file) of
    Left err -> printError err
    Right progr -> return progr
    