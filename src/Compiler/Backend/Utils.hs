module Backend.Utils where

import Control.Monad.State
import Control.Monad.Reader ( lift )
import qualified Control.Monad.Except as E ( throwError )

import qualified Data.Map as Map

import Latte.Abs

import Common
import Backend.Types

genType :: Type -> String
genType IntT = "i32"
genType BoolT = "i1"
genType CharT = "i8"
genType StringT = "i8*"
genType VoidT = "void"
genType (ArrayT t) = (genType t) ++ "*"

genVarType :: VarType -> String
genVarType (T t) = genType t
genVarType (Ref t) = (genType t) ++ "*"
genVarType (Arr (i, t)) = "[" ++ (show i) ++ " x " ++ (genType t) ++ "]"

genValType :: Val -> String
genValType (VInt _) = genType IntT
genValType (VFalse) = genType BoolT
genValType (VTrue) = genType BoolT
genValType (VLocal (t, _)) = genVarType t
genValType (VGlobal (t, _)) = genVarType t

genLocSymbol :: Symbol -> String
genLocSymbol (NumSym i) = "%_" ++ show i
genLocSymbol (StrSym s) = "%" ++ s

genGlobSymbol :: Symbol -> String
genGlobSymbol (NumSym i) = "@_" ++ show i
genGlobSymbol (StrSym s) = "@" ++ s

genVal :: Val -> String
genVal (VInt i) = show i
genVal (VFalse) = "false"
genVal (VTrue) = "true"
genVal (VLocal (_, sym)) = genLocSymbol sym
genVal (VGlobal (_, sym)) = genGlobSymbol sym

genTypedVal :: Val -> String
genTypedVal v = (genValType v) ++ " " ++ (genVal v)

genIdent :: Ident -> String
genIdent (Ident id) = id

dereference :: VarType -> VarType
dereference (Ref t) = (T t)

convVarType :: VarType -> Type
convVarType (T t) = t
convVarType (Ref t) = t

newLocalSym :: CM Symbol
newLocalSym = do
  newId <- gets (\(_, _, l, _) -> l)
  modify (\(locals, globals, l, g) -> (locals, globals, succ l, g))
  return $ NumSym newId

setLocal :: Ident -> Var -> CM ()
setLocal id var = modify (\(locals, globals, l, g) -> 
  (Map.insert id var locals, globals, l, g))

getLocal :: Ident -> CM Var
getLocal id = gets (\(locals, _, _, _) -> locals Map.! id)

newGlobalSym :: CM Symbol
newGlobalSym = do
  newId <- gets (\(_, _, _, g) -> g)
  modify (\(locals, globals, l, g) -> (locals, globals, l, succ g))
  return $ NumSym newId

setGlobal :: Ident -> Var -> CM ()
setGlobal id var = modify (\(locals, globals, l, g) -> 
  (locals, Map.insert id var globals, l, g))

getGlobal :: Ident -> CM Var
getGlobal id = gets (\(_, globals, _, _) -> globals Map.! id)

tryGetGlobal :: Ident -> CM (Maybe Var)
tryGetGlobal id = gets (\(_, globals, _, _) -> Map.lookup id globals)

genArgs :: [Val] -> String
genArgs [] = ""
genArgs (v:vs) = do
  let a = genTypedVal v
  let as = genArgs vs
  if as == ""
    then a
  else a ++ ", " ++ as

genGlobString :: Ident -> Var -> String
genGlobString (Ident s) (t, sym) = (genGlobSymbol sym) ++ 
  " = private constant " ++ (genVarType t) ++ " c\"" ++ s ++ "\\00\""

genGlobStrings :: Globals -> [String]
genGlobStrings globals = Map.elems (Map.mapWithKey genGlobString globals)

throwE :: String -> CM a
throwE s = lift $ E.throwError s
