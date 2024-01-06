module Backend.Utils where

import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Except as E ( throwError )

import qualified Data.Map as Map

import Latte.Abs
import Latte.Print (printTree)

import Common
import Backend.Types

genType :: Type -> String
genType IntT = "i32"
genType BoolT = "i1"
genType StringT = "i8*"
genType VoidT = "void"
genType (ArrayT t) = genArrayType ++ "*"
genType (ClassT id) = genClassType id ++ "*"
genType PtrT = "i8*"

genClassType :: Ident -> String
genClassType id = "%" ++ printTree id

genArrayType :: String
genArrayType = genClassType (Ident (restrictName "array"))

defaultVal :: Type -> Val
defaultVal t = case t of
  IntT -> VConst (CInt 0)
  BoolT -> VConst (CBool False)
  _ -> VConst CNull

genDefaultVal :: Type -> String
genDefaultVal t = case t of
  VoidT -> ""
  _ -> genVal $ defaultVal t

genTypedDefaultVal :: Type -> String
genTypedDefaultVal t = (genType t) ++ " " ++ genDefaultVal t

genVarType :: VarType -> String
genVarType (T t) = genType t
genVarType (Ref t) = (genType t) ++ "*"
genVarType (StringLit i) = "[" ++ (show i) ++ " x i8]"

valType :: Val -> VarType
valType (VConst (CInt _)) = T IntT
valType (VConst (CBool _)) = T BoolT
valType (VConst CNull) = T PtrT
valType (VLocal (t, _)) = t
valType (VGlobal (t, _)) = t

valToVar :: Val -> Var
valToVar (VLocal v) = v
valToVar (VGlobal v) = v

genValType :: Val -> String
genValType (VConst (CInt _)) = genType IntT
genValType (VConst (CBool _)) = genType BoolT
genValType (VConst CNull) = genType PtrT
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
genVal (VConst CNull) = "null"
genVal (VLocal (_, sym)) = genLocSymbol sym
genVal (VGlobal (_, sym)) = genGlobSymbol sym

genTypedVal :: Val -> String
genTypedVal v = (genValType v) ++ " " ++ (genVal v)

genIdent :: Ident -> String
genIdent (Ident id) = id

dereference :: VarType -> VarType
dereference (Ref t) = T t

convVarType :: VarType -> Type
convVarType (T t) = t

arrayElemType :: Type -> Type
arrayElemType (ArrayT t) = t

newLocalSym :: CM Symbol
newLocalSym = do
  newId <- gets (\(_, _, (l, _, _), _) -> l)
  modify (\(locals, globals, (l, g, lab), context) -> 
    (locals, globals, (succ l, g, lab), context))
  return $ NumSym newId

setLocal :: Ident -> Var -> CM ()
setLocal id var = modify (\(locals, globals, counters, context) -> 
  (Map.insert id var locals, globals, counters, context))

getLocal :: Ident -> CM Var
getLocal id = gets (\(locals, _, _, _) -> locals Map.! id)

tryGetLocal :: Ident -> CM (Maybe Var)
tryGetLocal id = gets (\(locals, _, _, _) -> Map.lookup id locals)

newGlobalSym :: CM Symbol
newGlobalSym = do
  newId <- gets (\(_, _, (_, g, _), _) -> g)
  modify (\(locals, globals, (l, g, lab), context) -> 
    (locals, globals, (l, succ g, lab), context))
  return $ NumSym newId

setGlobal :: Ident -> Var -> CM ()
setGlobal id var = modify (\(locals, globals, counters, context) -> 
  (locals, Map.insert id var globals, counters, context))

getGlobal :: Ident -> CM Var
getGlobal id = gets (\(_, globals, _, _) -> globals Map.! id)

tryGetGlobal :: Ident -> CM (Maybe Var)
tryGetGlobal id = gets (\(_, globals, _, _) -> Map.lookup id globals)

getAttributes :: Ident -> CM AttrEnv
getAttributes classId = do
  (_, classEnv) <- ask
  let (attributes, _, _) = classEnv Map.! classId
  return attributes

getMethods :: Ident -> CM MethodEnv
getMethods classId = do
  (_, classEnv) <- ask
  let (_, methods, _) = classEnv Map.! classId
  return methods

getSuper :: Ident -> CM Super
getSuper classId = do
  (_, classEnv) <- ask
  let (_, _, super) = classEnv Map.! classId
  return super  

newLabel :: CM Symbol
newLabel = do
  newId <- gets (\(_, _, (_, _, lab), _) -> lab)
  modify (\(locals, globals, (l, g, lab), context) -> 
    (locals, globals, (l, g, succ lab), context))
  return $ NumSym newId

isSuperClassOf :: Ident -> Ident -> CM Bool
isSuperClassOf superId subId  = do
  super <- getSuper subId
  case super of
    Just superSubId -> do
      if superId == superSubId
        then return True
      else isSuperClassOf superId superSubId 
    Nothing -> return False 

areTypesCompatible :: Type -> Type -> CM Bool
areTypesCompatible t1 t2 = do
  if t1 == t2
    then return True
  else if (isClassType t1) && (isClassType t2)
    then do
      let id1 = classIdent t1
      let id2 = classIdent t2
      isSuperClassOf id1 id2
  else return False

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
