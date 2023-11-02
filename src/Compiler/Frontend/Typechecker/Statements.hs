module Frontend.Typechecker.Statements where

import Control.Monad.Reader

import qualified Data.Map as Map

import Latte.Abs
import Latte.Print (printTree)

import Frontend.Types
import Frontend.Utils
import Frontend.Typechecker.Expressions (typeOfExpr, typeOfLVal, checkUnaryOp)

setVar :: Type -> Ident -> TM Env
setVar t id = do
  (varEnv, funcEnv) <- ask
  let varEnv' = Map.insert id t varEnv
  return (varEnv', funcEnv) 

checkSExp :: Expr -> TM Env  
checkSExp e = do
  typeOfExpr e
  ask

checkNoInit :: Pos -> Type -> Ident -> TM Env
checkNoInit pos t id = do
  (varEnv, _) <- ask
  if Map.member id varEnv
    then throwE pos $
      "variable " ++ printTree id ++ " has already been declared"
  else setVar t id

checkInit :: Pos -> Type -> Ident -> Expr -> TM Env
checkInit pos t id e = do
  exprT <- typeOfExpr e
  if exprT /= t
    then throwE pos $ 
      "wrong type of expression: " ++ printTree e ++
      "\nin definition of " ++ showType t ++ " " ++ printTree id
  else checkNoInit pos t id

checkDecl :: Type -> Item -> TM Env
checkDecl t i = case i of
  NoInit pos id -> checkNoInit pos t id
  Init pos id e -> checkInit pos t id e

checkDecls :: Type -> [Item] -> TM Env
checkDecls _ [] = ask
checkDecls t (i:is) = do
  env <- checkDecl t i
  local (const env) $ checkDecls t is

checkSDecl :: Pos -> TType -> [Item] -> TM Env
checkSDecl pos tt is = do
  let t = convType tt
  if t == VoidT
    then throwE pos $
      "declared void-type variable " ++ printTree is
  else if t == (ArrayT VoidT)
    then throwE pos $
      "declared void-type array " ++ printTree is
  else do
    env <- checkDecls t is
    return env

checkSAss :: Pos -> LVal -> Expr -> TM Env
checkSAss pos lv e = do
  varT <- typeOfLVal pos lv
  checkUnaryOp pos varT e
  ask

checkSIncrDecr :: Pos -> LVal -> TM Env
checkSIncrDecr pos lv = do
  t <- typeOfLVal pos lv
  if t /= IntT
    then throwE pos $
      "increment or decrement operator applied to non-int-type variable " ++ 
      printTree lv
  else ask

checkIfExpr :: Pos -> Expr -> TM ()
checkIfExpr pos e = do
  t <- typeOfExpr e
  if t /= BoolT
    then throwE pos $
      "condition in if statement is not bool-type: " ++ printTree e
  else return ()

checkSIf :: Pos -> Expr -> Stmt -> TM Env
checkSIf pos e s = do
  checkIfExpr pos e
  checkStmt s
  ask

checkSIfElse :: Pos -> Expr -> Stmt -> Stmt -> TM Env
checkSIfElse pos e sIf sElse = do
  checkSIf pos e sIf
  checkStmt sElse
  ask

checkSWhile :: Pos -> Expr -> Stmt -> TM Env
checkSWhile pos e s = do
  t <- typeOfExpr e
  if t /= BoolT
    then throwE pos $
      "condition in while statement is not bool-type: " ++ printTree e
  else do
    checkStmt s
    ask

checkSFor :: Pos -> TType -> Ident -> Expr -> Stmt -> TM Env
checkSFor pos tt id e s = do
  let t = convType tt
  exprT <- typeOfExpr e
  if exprT /= (ArrayT t)
    then throwE pos $
      "wrong type of expression in for loop: " ++ printTree e ++
      "\nexpected type: " ++ showType (ArrayT t)
  else do
    env <- setVar t id
    local (const env) $ checkStmt s
    ask

checkStmt :: Stmt -> TM Env
checkStmt s = case s of
  SEmpty _ -> ask
  SBlock _ b -> checkBlock b
  SExp _ e -> checkSExp e
  SDecl pos tt is -> checkSDecl pos tt is
  SAss pos lv e -> checkSAss pos lv e
  SIncr pos lv -> checkSIncrDecr pos lv
  SDecr pos lv -> checkSIncrDecr pos lv
  -- SRet pos e -> checkSRet pos e
  -- SVRet pos -> check SVRet pos
  SIf pos e s -> checkSIf pos e s
  SIfElse pos e sIf sElse -> checkSIfElse pos e sIf sElse
  SWhile pos e s -> checkSWhile pos e s
  SFor pos tt id e s -> checkSFor pos tt id e s
  _ -> ask

checkStmts :: [Stmt] -> TM Env
checkStmts [] = ask
checkStmts (s:ss) = do
  env <- checkStmt s
  local (const env) $ checkStmts ss

checkBlock :: Block -> TM Env
checkBlock (BBlock _ ss) = checkStmts ss
