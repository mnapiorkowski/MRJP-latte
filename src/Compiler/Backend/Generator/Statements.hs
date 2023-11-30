module Backend.Generator.Statements where

import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.DList as DList

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils
import Backend.Generator.Expressions (genExpr, genELVal)

genSExp :: Expr -> CM Code
genSExp e = do
  (code, _) <- genExpr e
  return code

genInit :: Type -> Ident -> Expr -> CM Code
genInit t id e = do
  (eCode, v) <- genExpr e
  sym <- newLocalSym
  let newVar = ((Ref t), sym)
  setLocal id newVar
  let alloca = DList.singleton $ (genLocSymbol sym) ++ " = alloca " ++ (genType t)
  let store = DList.singleton $ "store " ++ (genTypedVal v) ++ 
              ", " ++ (genTypedVal (VLocal newVar))
  return $ DList.concat [eCode, alloca, store]

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
    var@(t, sym) <- getLocal id
    case sym of
      NumSym _ -> do
        (code, v) <- genExpr e
        let store = DList.singleton $ "store " ++ (genTypedVal v) ++ 
                    ", " ++ (genTypedVal (VLocal var))
        return $ DList.concat [code, store]
      StrSym str -> genInit (convVarType t) id e
  -- LArr _ id e ->

genSIncrDecr :: Pos -> LVal -> AddOp -> CM Code
genSIncrDecr pos lv op = genSAss lv (EAdd pos (ELVal pos lv) op (ELitInt pos 1))

genSRet :: Expr -> CM Code
genSRet e = do
  (code, v) <- genExpr e
  let ret = DList.singleton $ "ret " ++ genTypedVal v
  return $ DList.concat [code, ret]

genSVRet :: CM Code
genSVRet = return $ DList.singleton "ret void"

genSIf :: Expr -> Stmt -> CM Code
genSIf e s = do
  (cond, v) <- genExpr e
  lIf <- newLabel
  ifCode <- genStmt s
  lEnd <- newLabel
  let brCond = DList.singleton $ "br " ++ (genTypedVal v) ++ ", " ++ 
          (genTypedLabel lIf) ++ ", " ++ (genTypedLabel lEnd)
  let label1 = DList.singleton $ genLabel lIf
  let label2 = DList.singleton $ genLabel lEnd
  let brEnd = DList.singleton $ "br " ++ (genTypedLabel lEnd)
  return $ DList.concat [cond, brCond, label1, ifCode, brEnd, label2]

genSIfElse :: Expr -> Stmt -> Stmt -> CM Code
genSIfElse e sIf sElse = do
  (cond, v) <- genExpr e
  lIf <- newLabel
  ifCode <- genStmt sIf
  lElse <- newLabel
  elseCode <- genStmt sElse
  lEnd <- newLabel
  let brCond = DList.singleton $ "br " ++ (genTypedVal v) ++ ", " ++ 
          (genTypedLabel lIf) ++ ", " ++ (genTypedLabel lElse)
  let brEnd = DList.singleton $ "br " ++ (genTypedLabel lEnd)
  let label1 = DList.singleton $ genLabel lIf
  let label2 = DList.singleton $ genLabel lElse
  let label3 = DList.singleton $ genLabel lEnd
  return $ DList.concat [
    cond, brCond, label1, ifCode, brEnd, label2, elseCode, brEnd, label3
    ]

genSWhile :: Expr -> Stmt -> CM Code
genSWhile e s = do
  lCond <- newLabel
  (cond, v) <- genExpr e
  lBody <- newLabel
  body <- genStmt s
  lEnd <- newLabel
  let brBody = DList.singleton $ "br " ++ (genTypedVal v) ++ ", " ++
              (genTypedLabel lBody) ++ ", " ++ (genTypedLabel lEnd)
  let brCond = DList.singleton $ "br " ++ (genTypedLabel lCond)
  let label1 = DList.singleton $ genLabel lCond
  let label2 = DList.singleton $ genLabel lBody
  let label3 = DList.singleton $ genLabel lEnd
  return $ DList.concat [
    brCond, label1, cond, brBody, label2, body, brCond, label3
    ]

genStmt :: Stmt -> CM Code
genStmt s = case s of
  SEmpty _ -> return DList.empty
  SBlock _ b -> genBlock b
  SExp _ e -> genSExp e
  SDecl _ tt is -> genSDecl tt is
  SAss _ lv e -> genSAss lv e
  SIncr pos lv -> genSIncrDecr pos lv (OPlus pos)
  SDecr pos lv -> genSIncrDecr pos lv (OMinus pos)
  SRet _ e -> genSRet e
  SVRet _ -> genSVRet
  SIf _ e s -> genSIf e s
  SIfElse _ e sIf sElse -> genSIfElse e sIf sElse
  SWhile _ e s -> genSWhile e s
  -- SFor pos tt id e s -> checkSFor pos tt id e s
  _ -> return DList.empty

genStmts :: [Stmt]-> Code -> CM Code
genStmts [] acc = return acc
genStmts (h:t) acc = do
  code <- genStmt h
  genStmts t (DList.append acc code)

genBlock :: Block -> CM Code
genBlock (BBlock _ ss) = do
  (locals, _, _) <- get
  code <- genStmts ss DList.empty
  (_, globals, counters) <- get
  put (locals, globals, counters)
  return code
