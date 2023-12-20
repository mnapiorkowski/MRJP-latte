module Frontend.Typechecker.Expressions where

import Control.Monad.Reader

import qualified Data.Map as Map

import Latte.Abs
import Latte.Print (printTree)

import Common
import Frontend.Types
import Frontend.Utils

checkUnaryOp :: Pos -> Type -> Expr -> TM ()
checkUnaryOp pos t e = do
  exprT <- typeOfExpr e
  if exprT /= t
    then throwE pos $
      "wrong type of expression: " ++ printTree e ++
      "\nexpected type: " ++ showType t
  else return ()

checkBinaryOp :: Pos -> Type -> Expr -> Expr -> TM ()
checkBinaryOp pos t e1 e2 = do
  checkUnaryOp pos t e1
  checkUnaryOp pos t e2

typeOfAddOp :: Pos -> Expr -> AddOp -> Expr -> TM Type
typeOfAddOp pos e1 op e2 = case op of
  OMinus pos -> checkBinaryOp pos IntT e1 e2 >> return IntT
  OPlus pos -> do
    t1 <- typeOfExpr e1
    t2 <- typeOfExpr e2
    if (t1 == IntT && t2 == IntT)
      then return IntT
    else if (t1 == StringT && t2 == StringT)
      then return StringT
    else throwE pos $
      "operator '+' applied to types " ++ showType t1 ++
      " and " ++ showType t2

typeOfMulOp :: Pos -> Expr -> MulOp -> Expr -> TM Type
typeOfMulOp pos e1 op e2 = checkBinaryOp pos IntT e1 e2 >> return IntT

typeOfRelOp :: Pos -> Expr -> RelOp -> Expr -> TM Type
typeOfRelOp pos e1 op e2 = case op of
  OEq pos' -> do
    t1 <- typeOfExpr e1
    t2 <- typeOfExpr e2
    if (t1 == t2)
      then return BoolT
    else throwE pos $
      "equality operator applied to different types: " ++ 
      showType t1 ++ " and " ++ showType t2
  ONeq pos' -> typeOfRelOp pos e1 (OEq pos') e2 
  _ -> checkBinaryOp pos IntT e1 e2 >> return BoolT

typeOfVar :: Pos -> Ident -> TM Type
typeOfVar pos id = do
  varEnv <- getVarEnv
  if Map.notMember id varEnv
    then throwE pos $
      "variable " ++ printTree id ++ " is not declared"
  else return $ varEnv Map.! id

typeOfLVal :: Pos -> LVal -> TM Type
typeOfLVal pos lv = case lv of
  LVar pos id -> typeOfVar pos id
  LArr pos eArr eAt -> do
    arrT <- typeOfExpr eArr
    atT <- typeOfExpr eAt
    if not (isArrayType arrT)
      then throwE pos $ 
        "operator [] applied to non-array-type expression: " ++ printTree eArr
    else if atT /= IntT
      then throwE pos $
        "array index is not int-type: " ++ printTree eAt
    else return $ typeOfArrayElem arrT
  LAttr pos e id -> do
    t <- typeOfExpr e
    if (isArrayType t) && (id == Ident "length")
      then return IntT
    else if not (isClassType t)
      then throwE pos $ "type " ++ showType t ++ 
        " does not have the attribute " ++ printTree id
    else do
      classEnv <- getClassEnv
      let attributes = classEnv Map.! (classIdent t)
      if Map.notMember id attributes
        then throwE pos $ "class " ++ showType t ++ 
          " does not have the attribute " ++ printTree id
      else return (attributes Map.! id)

-- typeOfSelf :: Pos -> TM Type
-- typeOfSelf pos = do
-- TODO: check if self is inside of a class

checkArg :: Pos -> Ident -> Type -> Expr -> TM ()
checkArg pos id t e = do
  argT <- typeOfExpr e
  if argT /= t
    then throwE pos $
      "types of arguments passed to function " ++ 
      printTree id ++ " do not match function's signature"
  else return ()

checkArgs :: Pos -> Ident -> [Type] -> [Expr] -> TM ()
checkArgs _ _ [] [] = return ()
checkArgs pos id [] _ = throwE pos $
  "too many arguments passed to function " ++ printTree id
checkArgs pos id _ [] = throwE pos $
  "too few arguments passed to function " ++ printTree id
checkArgs pos id (t:ts) (e:es) = do
  checkArg pos id t e
  checkArgs pos id ts es

typeOfCall :: Pos -> Ident -> [Expr] -> TM Type
typeOfCall pos id es = do
  funcEnv <- getFuncEnv
  if id == Ident "main"
    then throwE pos "cannot call main function"
  else if Map.notMember id funcEnv
    then throwE pos $
      "function " ++ printTree id ++ " is not defined"
  else do
    let (retT, paramTs) = funcEnv Map.! id
    checkArgs pos id paramTs es
    return retT

-- typeOfMetCall :: Pos -> Expr -> Ident -> [Expr] -> TM Type
-- typeOfMetCall pos e id es = do

typeOfNewObj :: Pos -> TType -> TM Type
typeOfNewObj pos tt = do
  let t = convType tt
  isValid <- isValidType t
  if (not (isClassType t)) || (not isValid)
    then throwE pos $
      "cannot construct object of type " ++ showType t
  else return t

typeOfNewArr :: Pos -> TType -> Expr -> TM Type
typeOfNewArr pos tt e = do
  sizeT <- typeOfExpr e
  if sizeT /= IntT
    then throwE pos "size of an array must be int-type"
  else do
    let t = convType tt
    isValid <- isValidType t
    if t == VoidT
      then throwE pos "cannot construct a void-type array"
    else if not isValid
      then throwE pos $ "cannot construct an array of type " ++ showType t
    else return (ArrayT t)

typeOfCast :: Pos -> TType -> Expr -> TM Type
typeOfCast pos tt e = do
  let t = convType tt
  isValid <- isValidType t
  valT <- typeOfExpr e
  if (isClassType t) && isValid && (valT == PtrT)
    then return t
  else throwE pos $ "cannot cast " ++ printTree e ++ " to " ++ showType t

typeOfExpr :: Expr -> TM Type
typeOfExpr e = case e of
  ELitInt _ _ -> return IntT
  ELitTrue _ -> return BoolT
  ELitFalse _ -> return BoolT
  EString _ _ -> return StringT
  ENull _ -> return PtrT
  ELVal pos lv -> typeOfLVal pos lv
  -- ESelf pos -> typeOfSelf pos
  ECall pos id es -> typeOfCall pos id es
  -- EMetCall pos e id es -> typeOfMetCall pos e id es
  ENewObj pos tt -> typeOfNewObj pos tt
  ENewArr pos tt e -> typeOfNewArr pos tt e
  ECast pos tt e -> typeOfCast pos tt e
  ENeg pos e -> checkUnaryOp pos IntT e >> return IntT
  ENot pos e -> checkUnaryOp pos BoolT e >> return BoolT
  EMul pos e1 op e2 -> typeOfMulOp pos e1 op e2
  EAdd pos e1 op e2 -> typeOfAddOp pos e1 op e2
  ERel pos e1 op e2 -> typeOfRelOp pos e1 op e2
  EAnd pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
  EOr pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
