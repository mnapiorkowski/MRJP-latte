module Backend.Generator.Program where

import System.IO

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.DList as DList
import Data.List (isPrefixOf, isSuffixOf)

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils
import Backend.Generator.Statements (genBlock, genInit')

indentLine :: String -> String
indentLine line =
  if null line ||
     any (`isPrefixOf` line) ["@", "declare", "define"]  ||
     any (`isSuffixOf` line) ["}", ":"]
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
  "declare i8* @malloc(i32)",
  "declare i8* @memset(i8*, i32, i32)",
  ""
  ]

arrayDecl :: Code
arrayDecl = DList.singleton $ genArrayType ++ " = type {" ++ 
  (genType PtrT) ++ ", " ++ (genType IntT) ++ "}"

genParams :: [Arg] -> String
genParams [] = ""
genParams ((AArg _ tt (Ident id)):ps) = do
  let pStr = (genType (convType tt)) ++ " " ++ (genLocSymbol (StrSym id))
  let psStr = genParams ps
  if psStr == ""
    then pStr
  else (pStr ++ ", " ++ psStr)

genStoreParam :: Arg -> CM Code
genStoreParam (AArg _ tt id@(Ident s)) = do
  let t = convType tt
  genInit' t id $ (genType t) ++ " " ++ (genLocSymbol (StrSym s))

genStoreParams :: [Arg] -> Code -> CM Code
genStoreParams [] acc = return acc
genStoreParams (a:as) acc = do
  code <- genStoreParam a
  genStoreParams as (DList.append acc code)

genFunc :: TType -> Ident -> [Arg] -> Block -> CM Code
genFunc tt (Ident id) as b = do
  let t = convType tt
  modify (\(locals, globals, (_, g, _)) -> (locals, globals, (0, g, 0)))
  let ps = genParams as
  let open = DList.fromList [
            "define " ++ (genType t) ++ " " ++ (genGlobSymbol (StrSym id)) ++ 
            "(" ++ ps ++ ") {"
            ]
  storeParams <- genStoreParams as DList.empty
  (code, hasRet) <- genBlock b
  close <- if (not hasRet) 
    then return $ DList.fromList [ 
        "ret " ++ (genTypedDefaultVal t), 
        "}"
        ]
    else return $ DList.singleton "}"
  return $ DList.concat [open, storeParams, code, close]

genAttribute :: CMember -> CM String
genAttribute m = case m of
  CAttr _ tt id -> return $ genType (convType tt)
  CMethod _ tt id as b -> return ""

genAttributes' :: [CMember] -> CM String
genAttributes' [] = return ""
genAttributes' (m:ms) = do
  mStr <- genAttribute m
  msStr <- genAttributes' ms
  if msStr == ""
    then return mStr
  else return $ mStr ++ ", " ++ msStr

genAttributes :: ClassBlock -> CM String
genAttributes (CBlock _ ms) = genAttributes' ms

genClass :: Ident -> ClassBlock -> CM Code
genClass id cb = do
  as <- genAttributes cb
  return $ DList.singleton $ (genClassType id) ++ " = type {" ++ as ++ "}"

genTopDef :: TopDef -> CM Code
genTopDef d = case d of
  FnDef _ tt id as b -> genFunc tt id as b
  ClassDef _ id cb -> genClass id cb

genTopDefs :: [TopDef] -> Code -> Code -> CM Code
genTopDefs [] cs fs = return $ DList.append cs fs
genTopDefs (d:ds) cs fs = case d of
  FnDef _ tt id as b -> do
    code <- genFunc tt id as b
    genTopDefs ds cs (DList.concat [fs, (DList.singleton ""), code])
  ClassDef _ id cb -> do
    code <- genClass id cb
    genTopDefs ds (DList.concat [cs, (DList.singleton ""), code]) fs

genProgr :: Program -> CM Code
genProgr (Progr pos ds) = genTopDefs ds DList.empty DList.empty

compile :: Program -> (FuncEnv, ClassEnv) -> IO String
compile p env = do
  let initState = (Map.empty, Map.empty, (0, 0, 0))
  let res = runExcept $ runReaderT (runStateT (genProgr p) initState) env
  case res of
    Left err -> printError ("compilation error " ++ err)
    Right (code, (_, globals, _)) -> do
      strings <- if Map.null globals
        then return DList.empty
      else return $ DList.append (DList.fromList $ genGlobStrings globals)
                    (DList.singleton "") 
      return $ indent $ DList.concat [declarations, strings, arrayDecl, code]
      