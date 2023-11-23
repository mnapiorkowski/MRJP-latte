module Backend.Generator.Program where

import System.IO

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.DList as DList
import Data.List (isPrefixOf)

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils
import Backend.Generator.Statements (genBlock)

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
  "declare i8* @concatStrings(i8*, i8*)",
  ""
  ]

genParam :: Arg -> CM String
genParam (AArg pos tt id@(Ident str)) = do
  let t = T $ convType tt
  let sym = StrSym str
  let newVar = (t, sym)
  setLocal id newVar
  return $ genTypedVal (VLocal newVar)

genParams :: [Arg] -> CM String
genParams [] = return ""
genParams (p:ps) = do
  pStr <- genParam p
  psStr <- genParams ps
  if psStr == ""
    then return pStr
  else return $ pStr ++ ", " ++ psStr

genFunc :: Pos -> TType -> Ident -> [Arg] -> Block -> CM Code
genFunc p tt (Ident id) as b = do
  let t = convType tt
  modify (\(locals, globals, _, g) -> (locals, globals, 0, g))
  ps <- genParams as
  code <- genBlock b
  let open = [
            "", 
            "define " ++ (genType t) ++ " " ++ (genGlobSymbol (StrSym id)) ++ 
            "(" ++ ps ++ ") {"
            ]
  let lastInstr = last $ DList.toList code
  let close | (t == VoidT) && (lastInstr /= "ret void") = ["ret void", "}"]
            | otherwise = ["}"]
  return $ DList.concat [DList.fromList open, code, DList.fromList close]

genTopDef :: TopDef -> CM Code
genTopDef d = case d of
  FnDef pos tt id as b -> genFunc pos tt id as b

genTopDefs :: [TopDef] -> Code -> CM Code
genTopDefs [] acc = return acc
genTopDefs (d:ds) acc = do
  code <- genTopDef d
  genTopDefs ds (DList.append acc code)

genProgr :: Program -> CM Code
genProgr (Progr pos ds) = genTopDefs ds DList.empty

compile :: Program -> FuncEnv -> IO String
compile p env = do
  let initState = (Map.empty, Map.empty, 0, 0)
  let res = runExcept $ runReaderT (runStateT (genProgr p) initState) env
  case res of
    Left err -> printError ("compilation error " ++ err)
    Right (code, (_, globals, _, _)) -> do
      let strings = DList.fromList $ genGlobStrings globals
      return $ indent $ DList.concat [declarations, strings, code]
      