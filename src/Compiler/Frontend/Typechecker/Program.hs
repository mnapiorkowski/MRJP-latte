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
import Frontend.Typechecker.Statements (setVar, checkBlock)

typeOfParam :: Arg -> TM Type
typeOfParam (AArg _ tt _) = return $ convType tt

typeOfParams :: [Arg] -> TM [Type]
typeOfParams [] = return []
typeOfParams (p:ps) = do
  t <- typeOfParam p
  ts <- typeOfParams ps
  return (t:ts)

setFunc :: Type -> Ident -> [Type] -> TM Env
setFunc t id paramTs = do
  (varEnv, funcEnv, varsInBlock) <- ask
  let funcEnv' = Map.insert id (t, paramTs) funcEnv
  return (varEnv, funcEnv', varsInBlock) 

setFnDef :: Pos -> TType -> Ident -> [Arg] -> TM Env
setFnDef pos tt id as = do
  let t = convType tt
  (_, funcEnv, _) <- ask
  if Map.member id funcEnv
    then throwE pos $
      "function " ++ printTree id ++ " is already defined"
  else do
    paramTs <- typeOfParams as
    setFunc t id paramTs

setTopDef :: TopDef -> TM Env
setTopDef d = case d of
  FnDef pos tt id as _ -> setFnDef pos tt id as

setTopDefs :: [TopDef] -> TM Env
setTopDefs [] = ask
setTopDefs (d:ds) = do
  env <- setTopDef d
  local (const env) $ setTopDefs ds

checkParam :: Arg -> TM Env
checkParam (AArg pos tt id) = do
  let t = convType tt
  if t == VoidT
    then throwE pos $
      "function parameter " ++ printTree id ++ " is void-type"
  else do
    (varEnv, _, _) <- ask
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

mergeEnv :: Env -> TM Env
mergeEnv (varEnv, funcEnv, varsInBlock) = do
    (oldVarEnv, oldFuncEnv, oldVarsInBlock) <- ask
    let newVarEnv = Map.union oldVarEnv varEnv
    let newFuncEnv = Map.union oldFuncEnv funcEnv
    let newVarsInBlock = Set.union oldVarsInBlock varsInBlock
    return (newVarEnv, newFuncEnv, newVarsInBlock)

tryEval :: Expr -> Maybe Val
tryEval e = case e of
  ELitTrue _ -> Just $ BoolV True
  ELitFalse _ -> Just $ BoolV False
  ENot _ e -> case tryEval e of
    Just (BoolV b) -> Just $ BoolV (not b)
    Nothing -> Nothing
  _ -> Nothing

doesReturnOrLoopForever :: Stmt -> Bool
doesReturnOrLoopForever s = case s of
  SRet _ _ -> True
  SBlock _ (BBlock _ ss) -> doReturnOrLoopForever ss
  SExp _ (EApp _ (Ident "error") []) -> True
  SIf _ e s -> do
    case tryEval e of
      Just (BoolV b) -> do
        if b == True    -- expression evaluates to True
          then doesReturnOrLoopForever s
        else False      -- expression evaluates to False
      Nothing -> False  -- could not evaluate expression 
  SIfElse _ e sIf sElse -> do
    case tryEval e of
      Just (BoolV b) -> do
        if b == True
          then doesReturnOrLoopForever sIf
        else doesReturnOrLoopForever sElse
      Nothing -> (doesReturnOrLoopForever sIf) && (doesReturnOrLoopForever sElse)
  SWhile _ e s -> do
    case tryEval e of
      Just (BoolV b) -> b
      Nothing -> False
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

checkFnDef :: Pos -> Ident -> [Arg] -> Block -> TM ()
checkFnDef pos id ps b = do
  env@(_, funcEnv, _) <- ask
  let (t, paramTs) = funcEnv Map.! id
  put (id)
  env1 <- local (const (Map.empty, Map.empty, Set.empty)) $ checkParams ps
  env2 <- local (const env1) $ setFunc t id paramTs -- recursion
  env3 <- local (const env) $ mergeEnv env2
  local (const env3) $ checkBlock b
  if t /= VoidT
    then analyseFlow b id
  else return ()

checkTopDef :: TopDef -> TM ()
checkTopDef d = case d of
  FnDef pos tt id as b -> checkFnDef pos id as b

checkTopDefs :: [TopDef] -> TM ()
checkTopDefs [] = return ()
checkTopDefs (d:ds) = do
  checkTopDef d
  checkTopDefs ds

checkProgr :: Program -> TM FuncEnv
checkProgr (Progr pos ds) = do
  env@(_, funcEnv, _) <- setTopDefs ds
  local (const env) $ checkTopDefs ds
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
        (Ident "concatStrings", (StringT, [StringT, StringT]))
        ]
  let initContext = (Ident "")
  let initEnv = (initVarEnv, initFuncEnv, Set.empty)
  let res = runExcept $ runReaderT (evalStateT (checkProgr p) initContext) initEnv
  case res of
    Left err -> printError ("semantic error " ++ err)
    Right env -> return env
