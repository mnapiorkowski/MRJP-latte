module Frontend.Utils where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Abs
import Latte.Print (printTree)

import Common
import Frontend.Types

showType :: Type -> String
showType IntT = "int"
showType BoolT = "bool"
showType StringT = "string"
showType VoidT = "void"
showType (ArrayT t) = (showType t) ++ "[]"
showType (ClassT id) = printTree id
showType PtrT = "null"

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

getAttributes :: Ident -> TM AttrEnv
getAttributes classId = do
  classEnv <- getClassEnv
  let (attributes, _, _) = classEnv Map.! classId
  return attributes

getMethods :: Ident -> TM MethodEnv
getMethods classId = do
  classEnv <- getClassEnv
  let (_, methods, _) = classEnv Map.! classId
  return methods

getSuper :: Ident -> TM Super
getSuper classId = do
  classEnv <- getClassEnv
  let (_, _, super) = classEnv Map.! classId
  return super  

getVarsInBlock :: TM VarsInBlock
getVarsInBlock = do
  (_, _, _, varsInBlock) <- ask
  return varsInBlock

setVar :: Ident -> Type -> TM Env
setVar id t = do
  (varEnv, funcEnv, classEnv, varsInBlock) <- ask
  let varEnv' = Map.insert id t varEnv
  let varsInBlock' = Set.insert id varsInBlock
  return (varEnv', funcEnv, classEnv, varsInBlock')

setFunc :: Ident -> Type -> [Type] -> TM Env
setFunc id t paramTs = do
  (varEnv, funcEnv, classEnv, varsInBlock) <- ask
  let funcEnv' = Map.insert id (t, paramTs) funcEnv
  return (varEnv, funcEnv', classEnv, varsInBlock)

setClass :: Ident -> AttrEnv -> MethodEnv -> Super -> TM Env
setClass id attributes methods super = do
  (varEnv, funcEnv, classEnv, varsInBlock) <- ask
  let classEnv' = Map.insert id (attributes, methods, super) classEnv
  return (varEnv, funcEnv, classEnv', varsInBlock)

isValidType :: Type -> TM Bool
isValidType (ClassT id) = do
  classEnv <- getClassEnv
  return (Map.member id classEnv)
isValidType (ArrayT t) = isValidType t
isValidType VoidT = return False
isValidType _ = return True

isSuperClassOf :: Ident -> Ident -> TM Bool
isSuperClassOf superId subId  = do
  super <- getSuper subId
  case super of
    Just superSubId -> do
      if superId == superSubId
        then return True
      else isSuperClassOf superId superSubId 
    Nothing -> return False 

areTypesCompatible :: Type -> Type -> TM Bool
areTypesCompatible t1 t2 = do
  if t1 == t2
    then return True
  else if (isClassType t1) && (isClassType t2)
    then do
      let id1 = classIdent t1
      let id2 = classIdent t2
      isSuperClassOf id1 id2
  else return False

throwE :: Pos -> String -> TM a
throwE pos s = lift $ E.throwError (posStr pos ++ s)
