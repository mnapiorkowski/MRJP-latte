module Backend.Utils where

import Control.Monad.State

import qualified Data.Map as Map

import Latte.Abs

import Common
import Backend.Types

genType :: Type -> String
genType IntT = "i32"
genType BoolT = "i1"
genType StringT = "i8*"
genType VoidT = "void"
genType (ArrayT t) = (genType t) ++ "*"

genRegType :: RegType -> String
genRegType (T t) = genType t
genRegType (Ref t) = (genType t) ++ "*"

genValType :: Val -> String
genValType (VInt _) = genType IntT
genValType (VFalse) = genType BoolT
genValType (VTrue) = genType BoolT
genValType (VVar (t, r)) = genRegType t

genReg :: Register -> String
genReg (Reg r) = "%_" ++ show r
genReg (RegArg s) = "%" ++ s

genVal :: Val -> String
genVal (VInt i) = show i
genVal (VFalse) = "false"
genVal (VTrue) = "true"
genVal (VVar (_, r)) = genReg r

genTypedVal :: Val -> String
genTypedVal v = (genValType v) ++ " " ++ (genVal v)

genIdent :: Ident -> String
genIdent (Ident id) = id

dereference :: RegType -> RegType
dereference (Ref t) = (T t)

newReg :: CM Register
newReg = do
  newLoc <- gets (\(_, loc) -> loc)
  modify (\(state, loc) -> (state, succ loc))
  return $ Reg newLoc

setVar :: Ident -> Var -> CM ()
setVar id var = modify (\(state, nextLoc) -> 
  (Map.insert id var state, nextLoc))

getVar :: Ident -> CM Var
getVar id = gets (\(state, _) -> state Map.! id)

genArgs :: [Val] -> String
genArgs [] = ""
genArgs (v:vs) = do
  let a = genTypedVal v
  let as = genArgs vs
  if as == ""
    then a
  else a ++ ", " ++ as
