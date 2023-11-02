module Frontend.Typechecker.Program where

import System.IO

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map as Map

import Latte.Abs
import Latte.Print (printTree)

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
  (varEnv, funcEnv) <- ask
  let funcEnv' = Map.insert id (t, paramTs) funcEnv
  return (varEnv, funcEnv') 

setFnDef :: Pos -> TType -> Ident -> [Arg] -> TM Env
setFnDef pos tt id as = do
  let t = convType tt
  (_, funcEnv) <- ask
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
    (varEnv, _) <- ask
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
mergeEnv (varEnv, funcEnv) = do
    (oldVarEnv, oldFuncEnv) <- ask
    let newVarEnv = Map.union oldVarEnv varEnv
    let newFuncEnv = Map.union oldFuncEnv funcEnv
    return (newVarEnv, newFuncEnv)

checkFnDef :: Pos -> Ident -> [Arg] -> Block -> TM ()
checkFnDef pos id ps b = do
  env@(_, funcEnv) <- ask
  let (t, paramTs) = funcEnv Map.! id
  env1 <- local (const (Map.empty, Map.empty)) $ checkParams ps
  env2 <- local (const env1) $ setFunc t id paramTs -- recursion
  env3 <- local (const env) $ mergeEnv env2
  env4 <- local (const env3) $ checkBlock b
  -- env5 <- case r of
  --   Turnback _ _ -> local (const env4) $ checkBlock (reverseBlock b)
  --   VTurnback _ -> local (const env4) $ checkBlock (reverseBlock b)
  --   _ -> return env4
  -- retT <- local (const env5) $ typeofRet r
  -- if retT /= t
  --   then throwE pos $
  --     "return type of function '" ++ printTree id ++
  --     "' does not match function's signature"
  -- else return ()
  return ()

checkTopDef :: TopDef -> TM ()
checkTopDef d = case d of
  FnDef pos tt id as b -> checkFnDef pos id as b

checkTopDefs :: [TopDef] -> TM ()
checkTopDefs [] = return ()
checkTopDefs (d:ds) = do
  checkTopDef d
  checkTopDefs ds

checkProgr :: Program -> TM ()
checkProgr (Progr pos ds) = do
  env@(_, funcEnv) <- setTopDefs ds
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
    else return ()

typecheck :: Program -> IO ()
typecheck p = do
  let initVarEnv = Map.empty
  let initFuncEnv = Map.fromList [
        (Ident "printInt", (VoidT, [IntT])),
        (Ident "printString", (VoidT, [StringT])),
        (Ident "error", (VoidT, [])),
        (Ident "readInt", (IntT, [])),
        (Ident "readString", (StringT, []))
        ]
  let res = runExcept $ runReaderT (checkProgr p) (initVarEnv, initFuncEnv)
  case res of
    Left err -> printError ("semantic error " ++ err)
    Right _ -> return ()
