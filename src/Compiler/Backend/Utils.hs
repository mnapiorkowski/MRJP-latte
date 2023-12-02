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

genDefaultValue :: Type -> String
genDefaultValue IntT = "i32 0"
genDefaultValue BoolT = "i1 false"
genDefaultValue CharT = "i8 0"
genDefaultValue StringT = "i8* null"
genDefaultValue VoidT = "void"
genDefaultValue (ArrayT t) = (genType t) ++ "* null"

genVarType :: VarType -> String
genVarType (T t) = genType t
genVarType (Ref t) = (genType t) ++ "*"
genVarType (Arr (i, t)) = "[" ++ (show i) ++ " x " ++ (genType t) ++ "]"

genValType :: Val -> String
genValType (VConst (CInt _)) = genType IntT
genValType (VConst (CBool _)) = genType BoolT
genValType (VLocal (t, _)) = genVarType t
genValType (VGlobal (t, _)) = genVarType t

genLocSymbol :: Symbol -> String
genLocSymbol (NumSym i) = "%_" ++ show i
genLocSymbol (StrSym s) = "%" ++ s

genGlobSymbol :: Symbol -> String
genGlobSymbol (NumSym i) = "@_" ++ show i
genGlobSymbol (StrSym s) = "@" ++ s

genLabel :: Symbol -> String
genLabel (NumSym i) = "L" ++ show i ++ ":"
genLabel (StrSym s) = "L" ++ s ++ ":"

genArgLabel :: Symbol -> String
genArgLabel (NumSym i) = "%L" ++ show i
genArgLabel (StrSym s) = "%L" ++ s

genTypedLabel :: Symbol -> String
genTypedLabel s = "label " ++ genArgLabel s

genVal :: Val -> String
genVal (VConst (CInt i)) = show i
genVal (VConst (CBool False)) = "false"
genVal (VConst (CBool True)) = "true"
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
  newId <- gets (\(_, _, (l, _, _)) -> l)
  modify (\(locals, globals, (l, g, lab)) -> 
    (locals, globals, (succ l, g, lab)))
  return $ NumSym newId

setLocal :: Ident -> Var -> CM ()
setLocal id var = modify (\(locals, globals, counters) -> 
  (Map.insert id var locals, globals, counters))

getLocal :: Ident -> CM Var
getLocal id = gets (\(locals, _, _) -> locals Map.! id)

newGlobalSym :: CM Symbol
newGlobalSym = do
  newId <- gets (\(_, _, (_, g, _)) -> g)
  modify (\(locals, globals, (l, g, lab)) -> 
    (locals, globals, (l, succ g, lab)))
  return $ NumSym newId

setGlobal :: Ident -> Var -> CM ()
setGlobal id var = modify (\(locals, globals, counters) -> 
  (locals, Map.insert id var globals, counters))

getGlobal :: Ident -> CM Var
getGlobal id = gets (\(_, globals, _) -> globals Map.! id)

tryGetGlobal :: Ident -> CM (Maybe Var)
tryGetGlobal id = gets (\(_, globals, _) -> Map.lookup id globals)

newLabel :: CM Symbol
newLabel = do
  newId <- gets (\(_, _, (_, _, lab)) -> lab)
  modify (\(locals, globals, (l, g, lab)) -> 
    (locals, globals, (l, g, succ lab)))
  return $ NumSym newId

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
  " = private constant " ++ (genVarType t) ++ " c\"" ++ escape s ++ "\\00\""
  where
    escape [] = []
    escape ('\t' : t) = '\\' : '0' : '9' : escape t
    escape ('\n' : t) = '\\' : '0' : 'A' : escape t
    escape ('"' : t) = '\\' : '2' : '2' : escape t
    escape ('\\' : t) = '\\': '5' : 'C' : escape t
    escape (h : t) = h : escape t

genGlobStrings :: Globals -> [String]
genGlobStrings globals = Map.elems (Map.mapWithKey genGlobString globals)

throwE :: String -> CM a
throwE s = lift $ E.throwError s
