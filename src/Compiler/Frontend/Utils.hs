module Frontend.Utils where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Abs

import Common
import Frontend.Types

showType :: Type -> String
showType IntT = "int"
showType BoolT = "bool"
showType StringT = "string"
showType VoidT = "void"
showType (ArrayT t) = (showType t) ++ "[]"
showType (ClassT (Ident id)) = id

getVarEnv :: TM VarEnv
getVarEnv = do
  (varEnv, _, _, _) <- ask
  return varEnv

getFuncEnv :: TM FuncEnv
getFuncEnv = do
  (_, funcEnv, _, _) <- ask
  return funcEnv

getClassEnv :: TM ClassEnv
getClassEnv = do
  (_, _, classEnv, _) <- ask
  return classEnv

getVarsInBlock :: TM VarsInBlock
getVarsInBlock = do
  (_, _, _, varsInBlock) <- ask
  return varsInBlock

setVar :: Type -> Ident -> TM Env
setVar t id = do
  (varEnv, funcEnv, classEnv, varsInBlock) <- ask
  let varEnv' = Map.insert id t varEnv
  let varsInBlock' = Set.insert id varsInBlock
  return (varEnv', funcEnv, classEnv, varsInBlock')

setFunc :: Type -> Ident -> [Type] -> TM Env
setFunc t id paramTs = do
  (varEnv, funcEnv, classEnv, varsInBlock) <- ask
  let funcEnv' = Map.insert id (t, paramTs) funcEnv
  return (varEnv, funcEnv', classEnv, varsInBlock)

setClass :: Ident -> VarEnv -> TM Env
setClass id attributes = do
  (varEnv, funcEnv, classEnv, varsInBlock) <- ask
  let classEnv' = Map.insert id attributes classEnv
  return (varEnv, funcEnv, classEnv', varsInBlock)

isValidType :: Type -> TM Bool
isValidType (ClassT id) = do
  classEnv <- getClassEnv
  return (Map.member id classEnv)
isValidType (ArrayT t) = isValidType t
isValidType _ = return True

throwE :: Pos -> String -> TM a
throwE pos s = lift $ E.throwError (posStr pos ++ s)
