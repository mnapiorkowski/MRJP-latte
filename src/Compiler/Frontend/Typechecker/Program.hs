module Frontend.Typechecker.Program where

import System.IO

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Abs
import Latte.Print (printTree)

import Common
import Frontend.Types
import Frontend.Utils
import Frontend.Typechecker.Statements (checkBlock)

setFuncId :: Pos -> Ident -> TM Env
setFuncId pos id = do
  funcEnv <- getFuncEnv
  if Map.member id funcEnv
    then throwE pos $
      "function " ++ printTree id ++ " is already defined"
  else setFunc PtrT id []

setClassId :: Pos -> Ident -> TM Env
setClassId pos id = do
  classEnv <- getClassEnv
  if Map.member id classEnv
    then throwE pos $
      "class " ++ printTree id ++ " is already defined"
  else setClass id Map.empty

setTopDef :: TopDef -> TM Env
setTopDef d = case d of
  FnDef pos _ id _ _ -> setFuncId pos id
  ClassDef pos id _ -> setClassId pos id

setTopDefs :: [TopDef] -> TM Env
setTopDefs [] = ask
setTopDefs (d:ds) = do
  env <- setTopDef d
  local (const env) $ setTopDefs ds

typeOfParam :: Arg -> TM Type
typeOfParam (AArg pos tt id) = do
  let t = convType tt
  isValid <- isValidType t
  if not isValid
    then throwE pos $ "invalid type " ++ showType t ++ " of function parameter"
  else if t == VoidT
    then throwE pos $
      "function parameter " ++ printTree id ++ " cannot be void-type"
  else return t

typeOfParams :: [Arg] -> TM [Type]
typeOfParams [] = return []
typeOfParams (p:ps) = do
  t <- typeOfParam p
  ts <- typeOfParams ps
  return (t:ts)

checkFuncSignature :: Pos -> TType -> Ident -> [Arg] -> TM Env
checkFuncSignature pos tt id ps = do
  let t = convType tt
  isValid <- isValidType t
  if not isValid
    then throwE pos $ "function returns invalid type " ++ showType t
  else do
    paramTs <- typeOfParams ps
    setFunc t id paramTs

checkMembers' :: [CMember] -> VarEnv -> TM VarEnv
checkMembers' [] attributes = return attributes
checkMembers' (m:ms) attributes = case m of
  CAttr pos tt id -> do
    if Map.member id attributes
      then throwE pos $
        "class member " ++ printTree id ++ " is already declared in this class"
    else do
      let t = convType tt
      isValid <- isValidType t
      if isValid
        then checkMembers' ms (Map.insert id t attributes)
      else throwE pos $
        "invalid type " ++ showType t ++ " of class attribute"
  CMethod pos tt id as b -> checkMembers' ms attributes -- TODO: check methods signatures

checkMembers :: ClassBlock -> TM VarEnv
checkMembers (CBlock _ ms) = checkMembers' ms Map.empty

checkClassMembers :: Ident -> ClassBlock -> TM Env
checkClassMembers id cb = do
  attributes <- checkMembers cb
  setClass id attributes

checkTopDef1 :: TopDef -> TM Env
checkTopDef1 d = case d of
  FnDef pos tt id as _ -> checkFuncSignature pos tt id as
  ClassDef _ id cb -> checkClassMembers id cb

checkTopDefs1 :: [TopDef] -> TM Env
checkTopDefs1 [] = ask
checkTopDefs1 (d:ds) = do
  env <- checkTopDef1 d
  local (const env) $ checkTopDefs1 ds

checkParam :: Arg -> TM Env
checkParam (AArg pos tt id) = do
  let t = convType tt
  varEnv <- getVarEnv
  if Map.member id varEnv
    then throwE pos $
      "function parameters have the same identifiers " ++ 
      printTree id
  else setVar t id

checkParams :: [Arg] -> TM Env
checkParams [] = ask
checkParams (p:ps) = do
  env <- checkParam p
  local (const env) $ checkParams ps

doesReturnOrLoopForever :: Stmt -> Bool
doesReturnOrLoopForever s = case s of
  SRet _ _ -> True
  SBlock _ (BBlock _ ss) -> doReturnOrLoopForever ss
  SExp _ (ECall _ (Ident "error") []) -> True
  SIf _ e s -> do
    case tryEval e of
      Just (CBool b) -> do
        if b == True    -- expression evaluates to True
          then doesReturnOrLoopForever s
        else False      -- expression evaluates to False
      Nothing -> False  -- could not evaluate expression 
  SIfElse _ e sIf sElse -> do
    case tryEval e of
      Just (CBool b) -> do
        if b == True
          then doesReturnOrLoopForever sIf
        else doesReturnOrLoopForever sElse
      Nothing -> (doesReturnOrLoopForever sIf) && (doesReturnOrLoopForever sElse)
  SWhile _ e s -> do
    case tryEval e of
      Just (CBool b) -> b
      Nothing -> False
  SFor _ _ _ _ s -> doesReturnOrLoopForever s
  _ -> False

doReturnOrLoopForever :: [Stmt] -> Bool
doReturnOrLoopForever [] = False
doReturnOrLoopForever (s:ss) = do
  if doesReturnOrLoopForever s
    then True
  else doReturnOrLoopForever ss

analyseFlow :: Block -> Ident -> TM ()
analyseFlow (BBlock pos ss) id = do
  if (doReturnOrLoopForever ss)
    then return ()
  else throwE pos $
    "function " ++ printTree id ++ " does not always return a value"

checkFuncBody :: Ident -> [Arg] -> Block -> TM ()
checkFuncBody id ps b = do
  env@(_, funcEnv, classEnv, _) <- ask
  put (id)
  env' <- local (const (Map.empty, funcEnv, classEnv, Set.empty)) $ 
          checkParams ps
  local (const env') $ checkBlock b
  let (t, paramTs) = funcEnv Map.! id
  if t /= VoidT
    then analyseFlow b id
  else return ()

checkMethods:: Pos -> Ident -> ClassBlock -> TM ()
checkMethods pos id cb = return () -- TODO

checkTopDef2 :: TopDef -> TM ()
checkTopDef2 d = case d of
  FnDef _ _ id as b -> checkFuncBody id as b
  ClassDef pos id cb -> checkMethods pos id cb

checkTopDefs2 :: [TopDef] -> TM ()
checkTopDefs2 [] = return ()
checkTopDefs2 (d:ds) = do
  checkTopDef2 d
  checkTopDefs2 ds

checkProgr :: Program -> TM FuncEnv
checkProgr (Progr pos ds) = do
  env <- setTopDefs ds
  env'@(_, funcEnv, _, _) <- local (const env) $ checkTopDefs1 ds
  local (const env') $ checkTopDefs2 ds
  let main = Ident "main"
  if Map.notMember main funcEnv
    then throwE pos $
      "main function is not defined"
  else do
    let (retT, paramTs) = funcEnv Map.! main
    if retT /= IntT
      then throwE pos $
        "main function must be int-type"
    else if not $ null paramTs
      then throwE pos $
        "main function cannot have any parameters"
    else return funcEnv

typecheck :: Program -> IO FuncEnv
typecheck p = do
  let initVarEnv = Map.empty
  let initFuncEnv = Map.fromList [
        (Ident "printInt", (VoidT, [IntT])),
        (Ident "printString", (VoidT, [StringT])),
        (Ident "error", (VoidT, [])),
        (Ident "readInt", (IntT, [])),
        (Ident "readString", (StringT, [])),
        (Ident "concatStrings", (StringT, [StringT, StringT])),
        (Ident "malloc", (PtrT, [IntT])),
        (Ident "memset", (PtrT, [PtrT, IntT, IntT]))
        ]
  let initClassEnv = Map.empty
  let initContext = (Ident "")
  let initEnv = (initVarEnv, initFuncEnv, initClassEnv, Set.empty)
  let res = runExcept $ 
              runReaderT (
                evalStateT (checkProgr p) initContext
              ) initEnv
  case res of
    Left err -> printError ("semantic error " ++ err)
    Right env -> return env
