module Frontend.Typechecker.Expressions where

import Control.Monad.Reader

import qualified Data.Map as Map

import Latte.Abs
import Latte.Print (printTree)

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
  (varEnv, _) <- ask
  if Map.notMember id varEnv
    then throwE pos $
      "variable " ++ printTree id ++ " is not declared"
  else return $ varEnv Map.! id

typeOfLVal :: Pos -> LVal -> TM Type
typeOfLVal pos lv = case lv of
  LVar pos id -> typeOfVar pos id
  LArr pos eId eAt -> do
    idT <- typeOfExpr eId
    atT <- typeOfExpr eAt
    if not (isArrayType idT)
      then throwE pos $ 
        "operator [] applied to non-array expression " ++ printTree eId
    else if atT /= IntT
      then throwE pos $
        "array index is not int-type: " ++ printTree eAt
    else return $ typeOfArrayElem idT

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

typeOfApp :: Pos -> Ident -> [Expr] -> TM Type
typeOfApp pos id es = do
  (_, funcEnv) <- ask
  if id == Ident "main"
    then throwE pos "cannot call main function"
  else if Map.notMember id funcEnv
    then throwE pos $
      "function " ++ printTree id ++ " is not defined"
  else do
    let (retT, paramTs) = funcEnv Map.! id
    checkArgs pos id paramTs es
    return retT

typeOfNewArr :: Pos -> TType -> Expr -> TM Type
typeOfNewArr pos tt e = do
  sizeT <- typeOfExpr e
  if sizeT /= IntT
    then throwE pos "size of an array must be int-type"
  else do
    let t = convType tt
    if t == VoidT
      then throwE pos "cannot construct a void-type array"
    else return (ArrayT t)

typeOfAttr :: Pos -> Expr -> Ident -> TM Type
typeOfAttr pos e id = do
  t <- typeOfExpr e
  if id /= Ident "length"
    then throwE pos $
      "unknown attribute: " ++ printTree id
  else if not (isArrayType t)
    then throwE pos $
      "tried to get length attribute from non-array expression " ++ 
      printTree e
  else return IntT

typeOfExpr :: Expr -> TM Type
typeOfExpr e = case e of
  ELitInt _ _ -> return IntT
  ELitTrue _ -> return BoolT
  ELitFalse _ -> return BoolT
  EString _ _ -> return StringT
  ELVal pos lv -> typeOfLVal pos lv
  EApp pos id es -> typeOfApp pos id es
  ENewArr pos tt e -> typeOfNewArr pos tt e
  EAttr pos e id -> typeOfAttr pos e id
  ENeg pos e -> checkUnaryOp pos IntT e >> return IntT
  ENot pos e -> checkUnaryOp pos BoolT e >> return BoolT
  EMul pos e1 op e2 -> typeOfMulOp pos e1 op e2
  EAdd pos e1 op e2 -> typeOfAddOp pos e1 op e2
  ERel pos e1 op e2 -> typeOfRelOp pos e1 op e2
  EAnd pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
  EOr pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
