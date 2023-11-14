module Backend.Generator.Program where

import System.IO

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.DList as DList
import Data.List (isPrefixOf)

import Latte.Abs

import Utils
import Backend.Types

indentLine :: String -> String
indentLine line =
  if null line ||
     any (`isPrefixOf` line) ["@", "}", "declare", "define"] 
  then line ++ "\n"
  else "\t" ++ line ++ "\n"

indent :: Code -> String
indent = DList.foldr ((++) . indentLine) ""

declarations :: Code
declarations = DList.fromList [
  "declare void @printInt(i32)",
  "declare void @printString(i8*)",
  "declare void @error()",
  "declare i32 @readInt()",
  "declare i8* @readString()",
  ""
  ]

header :: Code
header = DList.fromList [
  "define i32 @main() {"
  ]

footer :: Code
footer = DList.fromList [
  "ret i32 0",
  "}"
  ]

genProgr :: Program -> CM Code
genProgr (Progr pos ds) = do
  -- transStmts stmts DList.empty
  pure $ DList.fromList [
    "%x = call i8* @readString()", 
    "call void @printString(i8* %x)",
    "%i = call i32 @readInt()",
    "call void @printInt(i32 %i)",
    "call void @error()"
    ]

compileProgr :: Program -> CM String
compileProgr p = do
  code <- genProgr p
  pure $ indent $ DList.concat [declarations, header, code, footer]

compile :: Program -> IO String
compile p = do
  let initEnv = (Map.empty, 0)
  let res = runExcept $ evalStateT (compileProgr p) initEnv
  case res of
    Left err -> printError ("semantic error " ++ err)
    Right code -> return code
    