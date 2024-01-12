module Backend.Generator.Statements where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.DList as DList

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils
import Backend.Generator.Expressions (
  genExpr, genGetArrayPtr, genGetArrayElem, genGetArrayLength,
  genCheckAgainstVal, genCastToSuper
  )

genSExp :: Expr -> CM (Code, HasRet)
genSExp e = do
  (code, _) <- genExpr e
  return (code, False)

genStore :: Val -> Val -> CM Code
genStore v vWhere = do
  (cast, v') <- genCastToSuper v (convVarType $ dereference $ valType vWhere)
  let store = DList.singleton $ "store " ++ (genTypedVal v') ++ 
              ", " ++ (genTypedVal vWhere)
  return $ DList.concat [cast, store]

genInit' :: Type -> Ident -> Val -> CM Code
genInit' t id v = do
  sym <- newLocalSym
  let newVar = (Ref t, sym)
  setLocal id newVar
  let alloca = DList.singleton $ (genLocSymbol sym) ++ " = alloca " ++ 
              (genType t)
  store <- genStore v (VLocal newVar)
  return $ DList.concat [alloca, store]

genInit :: Type -> Ident -> Expr -> CM Code
genInit t id e = do
  (code, v) <- genExpr e
  init <- genInit' t id v
  return $ DList.concat [code, init]

genNoInit :: Type -> Ident -> CM Code
genNoInit t id = do
  sym <- newLocalSym
  let newVar = (Ref t, sym)
  setLocal id newVar
  let alloca = DList.singleton $ (genLocSymbol sym) ++ " = alloca " ++ 
              (genType t)
  let store = DList.singleton $ "store " ++ (genTypedDefaultVal t) ++ 
              ", " ++ (genTypedVal (VLocal newVar))
  return $ DList.concat [alloca, store]

genDecl :: Type -> Item -> CM Code
genDecl t i = case i of
  NoInit _ id -> genNoInit t id
  Init _ id e -> genInit t id e

genDecls :: Type -> [Item] -> Code -> CM Code
genDecls t [] acc = return acc
genDecls t (i:is) acc = do
  code <- genDecl t i
  genDecls t is (DList.append acc code)

genSDecl :: TType -> [Item] -> CM (Code, HasRet)
genSDecl tt is = do
  let t = convType tt
  code <- genDecls t is DList.empty
  return (code, False)

genSAss :: LVal -> Expr -> CM (Code, HasRet)
genSAss lv e = case lv of
  LVar _ id -> do
    local <- tryGetLocal id
    case local of
      Just var -> do
        (code, v) <- genExpr e
        store <- genStore v (VLocal var)
        return (DList.concat [code, store], False)
      Nothing -> genSAss (LAttr Nothing (ESelf Nothing) id) e
  LArr _ eArr eIdx -> do
    (arrCode, v) <- genExpr eArr
    (code, arr) <- genGetArrayPtr v
    (lengthCode, vLength) <- genGetArrayLength v
    (idx, vIdx) <- genExpr eIdx
    check <- genCheckAgainstVal vIdx "slt" (VConst (CInt 0))
    check2 <- genCheckAgainstVal vIdx "sge" vLength
    (elemCode, elemPtr) <- genGetArrayElem arr vIdx
    (exprCode, v) <- genExpr e
    store <- genStore v elemPtr
    return (DList.concat [
      arrCode, code, lengthCode, idx, check, check2, elemCode, exprCode, store
      ], False)
  LAttr _ eObj id -> do
    (objCode, vObj) <- genExpr eObj
    let t = convVarType $ valType vObj
    attributes <- getAttributes (classIdent t)
    let (num, attrT) = attributes Map.! id
    ptrSym <- newLocalSym
    let ptr = VLocal (Ref attrT, ptrSym)
    let gep = DList.singleton $ (genLocSymbol ptrSym) ++ 
              " = getelementptr " ++ (genClassType (classIdent t)) ++ ", " ++ 
              (genTypedVal vObj) ++ ", " ++ (genType IntT) ++ 
              " 0, " ++ (genType IntT) ++ " " ++ show num
    (code, v) <- genExpr e
    store <- genStore v ptr
    return $ (DList.concat [objCode, gep, code, store], False)

genSIncrDecr :: LVal -> AddOp -> CM (Code, HasRet)
genSIncrDecr lv op = genSAss lv 
  (EAdd Nothing (ELVal Nothing lv) op (ELitInt Nothing 1))

genSRet :: Expr -> CM (Code, HasRet)
genSRet e = do
  (code, v) <- genExpr e
  (_, _, _, (t)) <- get
  (cast, v') <- genCastToSuper v t
  let ret = DList.singleton $ "ret " ++ genTypedVal v'
  return (DList.concat [code, cast, ret], True)

genSVRet :: CM (Code, HasRet)
genSVRet = return (DList.singleton ("ret " ++ (genType VoidT)), True)

genSIf :: Expr -> Stmt -> CM (Code, HasRet)
genSIf e s = case tryEval e of
  Just (CBool False) -> return (DList.empty, False)
  Just (CBool True) -> genStmt s
  _ -> do
    (cond, v) <- genExpr e
    lIf <- newLabel
    (ifCode, hasRet) <- genStmt s
    lEnd <- newLabel
    let brCond = DList.singleton $ "br " ++ (genTypedVal v) ++ ", " ++ 
            (genTypedLabel lIf) ++ ", " ++ (genTypedLabel lEnd)
    let label1 = DList.singleton $ genLabel lIf
    let label2 = DList.singleton $ genLabel lEnd
    if hasRet
      then return (DList.concat [
        cond, brCond, label1, ifCode, label2
        ], False)
    else do
      let brEnd = DList.singleton $ "br " ++ (genTypedLabel lEnd)
      return (DList.concat [
        cond, brCond, label1, ifCode, brEnd, label2
        ], False)

genSIfElse :: Expr -> Stmt -> Stmt -> CM (Code, HasRet)
genSIfElse e sIf sElse = case tryEval e of
  Just (CBool False) -> genStmt sElse
  Just (CBool True) -> genStmt sIf
  _ -> do
    (cond, v) <- genExpr e
    lIf <- newLabel
    (ifCode, hasIfRet) <- genStmt sIf
    lElse <- newLabel
    (elseCode, hasElseRet) <- genStmt sElse
    let brCond = DList.singleton $ "br " ++ (genTypedVal v) ++ ", " ++ 
                (genTypedLabel lIf) ++ ", " ++ (genTypedLabel lElse)
    let label1 = DList.singleton $ genLabel lIf
    let label2 = DList.singleton $ genLabel lElse
    if (hasIfRet && hasElseRet)
      then return (DList.concat [
        cond, brCond, label1, ifCode, label2, elseCode
        ], True)
    else do
      lEnd <- newLabel
      let brEnd = DList.singleton $ "br " ++ (genTypedLabel lEnd)
      let label3 = DList.singleton $ genLabel lEnd
      return (DList.concat [
        cond, brCond, label1, ifCode, brEnd, label2, elseCode, brEnd, label3
        ], False)

genSWhile :: Expr -> Stmt -> CM (Code, HasRet)
genSWhile e s = case tryEval e of
  Just (CBool False) -> return (DList.empty, False)
  Just (CBool True) -> do
    (body, hasRet) <- genStmt s
    if hasRet
      then return (body, True)
    else do
      lBody <- newLabel
      lEnd <- newLabel
      let br = DList.singleton $ "br " ++ (genTypedLabel lBody)
      let label1 = DList.singleton $ genLabel lBody
      let label2 = DList.singleton $ genLabel lEnd
      return (DList.concat [br, label1, body, br, label2], False)
  _ -> do
    lCond <- newLabel
    (cond, v) <- genExpr e
    lBody <- newLabel
    (body, hasRet) <- genStmt s
    let label1 = DList.singleton $ genLabel lCond
    let label2 = DList.singleton $ genLabel lBody
    if hasRet
      then do
        let brCond = DList.singleton $ "br " ++ (genTypedVal v) ++ ", " ++ 
                    (genTypedLabel lCond) ++ ", " ++ (genTypedLabel lBody)
        return (DList.concat [
          cond, brCond, label1, body, label2
          ], False)
    else do
      lEnd <- newLabel
      let brBody = DList.singleton $ "br " ++ (genTypedVal v) ++ ", " ++
                  (genTypedLabel lBody) ++ ", " ++ (genTypedLabel lEnd)
      let brCond = DList.singleton $ "br " ++ (genTypedLabel lCond)
      let label3 = DList.singleton $ genLabel lEnd
      return (DList.concat [
        brCond, label1, cond, brBody, label2, body, brCond, label3
        ], False)

genSFor :: Type -> Ident -> Expr -> Stmt -> CM (Code, HasRet)
genSFor t id e s = do
  (code, v) <- genExpr e
  lBefore <- newLabel
  let brBefore = DList.singleton $ "br " ++ (genTypedLabel lBefore)
  let label1 = DList.singleton $ genLabel lBefore
  (lengthCode, length) <- genGetArrayLength v
  (arrCode, arr) <- genGetArrayPtr v
  lLoop <- newLabel
  let brLoop = DList.singleton $ "br " ++ (genTypedLabel lLoop)
  let label2 = DList.singleton $ genLabel lLoop
  counterSym <- newLocalSym
  incrSym <- newLocalSym
  let incr = DList.singleton $ (genLocSymbol incrSym) ++ " = add " ++ 
            (genType IntT) ++ " " ++ (genLocSymbol counterSym) ++ ", 1"
  (elemCode, elemVal) <- genGetArrayElem arr (VLocal (T IntT, counterSym))
  (cast, elemVal') <- genCastToSuper elemVal t

  (locals, _, _, _) <- get
  setLocal id (valToVar elemVal')
  (stmtCode, hasRet) <- genStmt s
  (_, globals, counters, context) <- get
  put (locals, globals, counters, context)

  lCheck <- newLabel
  let brCheck = DList.singleton $ "br " ++ (genTypedLabel lCheck)
  let label3 = DList.singleton $ genLabel lCheck
  let phi = DList.singleton $ (genLocSymbol counterSym) ++ " = phi " ++ 
            (genType IntT) ++ " [0, " ++ (genArgLabel lBefore) ++ "], [" ++
            (genLocSymbol incrSym) ++ ", " ++ (genArgLabel lCheck) ++ "]"

  icmpSym <- newLocalSym
  let icmp = DList.singleton $ (genLocSymbol icmpSym) ++ " = icmp sgt " ++
            (genTypedVal length) ++ ", " ++ (genLocSymbol incrSym)
  lAfter <- newLabel
  let brCond = DList.singleton $ "br " ++ (genType BoolT) ++ " " ++ 
              (genLocSymbol icmpSym) ++ ", " ++ (genTypedLabel lLoop) ++
              ", " ++ (genTypedLabel lAfter)
  let label4 = DList.singleton $ genLabel lAfter
  return (DList.concat [
    code, brBefore, label1, lengthCode, arrCode, brLoop, label2, phi, incr,
    elemCode, cast, stmtCode, brCheck, label3, icmp, brCond, label4
    ], False)

genStmt :: Stmt -> CM (Code, HasRet)
genStmt s = case s of
  SEmpty _ -> return (DList.empty, False)
  SBlock _ b -> genBlock b
  SExp _ e -> genSExp e
  SDecl _ tt is -> genSDecl tt is
  SAss _ lv e -> genSAss lv e
  SIncr _ lv -> genSIncrDecr lv (OPlus Nothing)
  SDecr _ lv -> genSIncrDecr lv (OMinus Nothing)
  SRet _ e -> genSRet e
  SVRet _ -> genSVRet
  SIf _ e s -> genSIf e s
  SIfElse _ e sIf sElse -> genSIfElse e sIf sElse
  SWhile _ e s -> genSWhile e s
  SFor _ tt id e s -> genSFor (convType tt) id e s

genStmts :: [Stmt]-> Code -> CM (Code, HasRet)
genStmts [] acc = return (acc, False)
genStmts (h:t) acc = do
  (code, hasRet) <- genStmt h
  let acc' = DList.append acc code
  if hasRet
    then return (acc', True)
  else genStmts t acc'

genBlock :: Block -> CM (Code, HasRet)
genBlock (BBlock _ ss) = do
  (locals, _, _, _) <- get
  res@(code, _) <- genStmts ss DList.empty
  (_, globals, counters, context) <- get
  put (locals, globals, counters, context)
  return res
