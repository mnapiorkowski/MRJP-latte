module Backend.Generator.Statements where

import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.DList as DList

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils
import Backend.Generator.Expressions (genExpr)

genSExp :: Expr -> CM Code
genSExp e = do
  (code, _) <- genExpr e
  return code

genInit :: Type -> Ident -> Expr -> CM Code
genInit t id e = do
  (eCode, v) <- genExpr e
  r <- newReg
  let newVar = ((Ref t), r)
  setVar id newVar
  let alloca = DList.singleton $ (genReg r) ++ " = alloca " ++ (genType t)
  let store = DList.singleton $ "store " ++ (genTypedVal v) ++ 
              ", " ++ (genTypedVal (VVar newVar))
  return $ DList.concat [
    eCode,
    alloca,
    store
    ]

genNoInit :: Pos -> Type -> Ident -> CM Code
genNoInit pos t id = case t of
  IntT -> genInit t id (ELitInt pos 0)
  BoolT -> genInit t id (ELitFalse pos)
  StringT -> genInit t id (EString pos "")
  -- ArrayT -> ...

genDecl :: Type -> Item -> CM Code
genDecl t i = case i of
  NoInit pos id -> genNoInit pos t id
  Init _ id e -> genInit t id e

genDecls :: Type -> [Item] -> Code -> CM Code
genDecls t [] acc = return acc
genDecls t (i:is) acc = do
  code <- genDecl t i
  genDecls t is (DList.append acc code)

genSDecl :: TType -> [Item] -> CM Code
genSDecl tt is = do
  let t = convType tt
  genDecls t is DList.empty

genSAss :: LVal -> Expr -> CM Code
genSAss lv e = case lv of
  LVar _ id -> do
    var <- getVar id
    (code, v) <- genExpr e
    let store = DList.singleton $ "store " ++ (genTypedVal v) ++ 
                ", " ++ (genTypedVal (VVar var))
    return $ DList.concat [code, store]
  -- LArr _ id e ->

genSRet :: Expr -> CM Code
genSRet e = do
  (code, v) <- genExpr e
  let ret = DList.singleton $ "ret " ++ genTypedVal v
  return $ DList.concat [code, ret]

genSVRet :: CM Code
genSVRet = return $ DList.singleton "ret void"

genStmt :: Stmt -> CM Code
genStmt s = case s of
  SEmpty _ -> return DList.empty
  SBlock _ b -> genBlock b
  SExp _ e -> genSExp e
  SDecl _ tt is -> genSDecl tt is
  SAss _ lv e -> genSAss lv e
  -- SIncr _ lv -> genSIncr lv
  -- SDecr pos lv -> checkSIncrDecr pos lv
  SRet _ e -> genSRet e
  SVRet _ -> genSVRet
  -- SIf pos e s -> checkSIf pos e s
  -- SIfElse pos e sIf sElse -> checkSIfElse pos e sIf sElse
  -- SWhile pos e s -> checkSWhile pos e s
  -- SFor pos tt id e s -> checkSFor pos tt id e s
  _ -> return DList.empty

genStmts :: [Stmt]-> Code -> CM Code
genStmts [] acc = return acc
genStmts (h:t) acc = do
  code <- genStmt h
  genStmts t (DList.append acc code)

genBlock :: Block -> CM Code
genBlock (BBlock _ ss) = do
  (state, _) <- get
  code <- genStmts ss DList.empty
  loc <- gets (\(_, loc) -> loc)
  put (state, loc)
  return code
