module Backend.Generator.Expressions where

import qualified Data.Map as Map
import qualified Data.DList as DList

import Control.Monad.Reader

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils

genEString :: String -> CM (Code, Val)
genEString s = do
  res <- tryGetGlobal (Ident s)
  (t, sym) <- case res of
    Just var -> return var
    Nothing -> do
      g <- newGlobalSym
      let newVar = (StringLit (1 + length s), g)
      setGlobal (Ident s) newVar
      return newVar
  sym' <- newLocalSym
  let bitcast = (genLocSymbol sym') ++ " = bitcast " ++ (genVarType t) ++ 
                "* " ++ (genGlobSymbol sym) ++ " to " ++ (genType StringT)
  return (DList.singleton bitcast, (VLocal ((T StringT), sym')))

genGetArrayLength :: Val -> CM (Code, Val)
genGetArrayLength arr = do
  let elemT = arrayElemType $ convVarType $ valType arr
  ptrSym <- newLocalSym
  let ptr = VLocal (Ref IntT, ptrSym)
  let gep = DList.singleton $ (genLocSymbol ptrSym) ++ " = getelementptr " ++ 
            "%array, " ++ (genTypedVal arr) ++ ", i32 0, i32 1"
  lengthSym <- newLocalSym
  let length = VLocal (T IntT, lengthSym)
  let loadArr = DList.singleton $ (genLocSymbol lengthSym) ++ 
                " = load " ++ (genType IntT) ++ ", " ++ (genTypedVal ptr)
  return (DList.concat [gep, loadArr], length)

genGetArrayPtr :: Val -> CM (Code, Val)
genGetArrayPtr arr = do
  let elemT = arrayElemType $ convVarType $ valType arr
  ptrSym <- newLocalSym
  let gep = DList.singleton $ (genLocSymbol ptrSym) ++ " = getelementptr " ++ 
            "%array, " ++ (genTypedVal arr) ++ ", i32 0, i32 0"
  arrSym <- newLocalSym
  let loadArr = DList.singleton $ (genLocSymbol arrSym) ++ 
                " = load " ++ (genType PtrT) ++ ", " ++ 
                (genVarType (Ref PtrT)) ++ " " ++ (genLocSymbol ptrSym)
  castSym <- newLocalSym
  let castVal = VLocal (Ref elemT, castSym)
  let bitcast = DList.singleton $ (genLocSymbol castSym) ++ 
                " = bitcast " ++ (genType PtrT) ++ " " ++ 
                (genLocSymbol arrSym) ++ " to " ++ (genVarType (Ref elemT))
  return (DList.concat [gep, loadArr, bitcast], castVal)

genGetArrayElem :: Val -> Val -> CM (Code, Val)
genGetArrayElem arr idx = do
  elemSym <- newLocalSym
  let elemT = convVarType $ dereference $ valType arr
  let elemPtr = VLocal (Ref elemT, elemSym)
  let gep = DList.singleton $ (genLocSymbol elemSym) ++ 
            " = getelementptr " ++ (genType elemT) ++ ", " ++ 
            (genTypedVal arr) ++ ", " ++ (genTypedVal idx)
  return (gep, elemPtr)

genLoadVar :: Ident -> CM (Code, Val)
genLoadVar id = do
  var@(t, sym) <- getLocal id
  case sym of
    NumSym _ -> do
      let t' = dereference t
      sym' <- newLocalSym
      let load = DList.singleton $ (genLocSymbol sym') ++ " = load " ++ 
                (genVarType t') ++ ", " ++ genTypedVal (VLocal var)
      return (load, (VLocal (t', sym')))
    StrSym _ -> return (DList.empty, (VLocal var))

genELVal :: LVal -> CM (Code, Val)
genELVal lv = case lv of
  LVar _ id -> genLoadVar id
  LArr _ eArr eAt -> do
    (arrCode, v) <- genExpr eArr
    (code, arr) <- genGetArrayPtr v
    (lengthCode, vLength) <- genGetArrayLength v
    (idx, vIdx) <- genExpr eAt
    check <- genCheckAgainstVal vIdx "slt" (VConst (CInt 0))
    check2 <- genCheckAgainstVal vIdx "sge" vLength
    (elemCode, elemPtr) <- genGetArrayElem arr vIdx
    let elemT = dereference $ valType elemPtr
    elemSym <- newLocalSym
    let elemVal = VLocal (elemT, elemSym)
    let loadElem = DList.singleton $ (genLocSymbol elemSym) ++ " = load " ++
                  (genVarType elemT) ++ ", " ++ (genTypedVal elemPtr)
    return (DList.concat [
      arrCode, code, lengthCode, idx, check, check2, elemCode, loadElem
      ], elemVal)

genEApp :: Ident -> [Expr] -> CM (Code, Val)
genEApp id es = do
  (code, vals) <- genExprs es (DList.empty, [])
  funcEnv <- ask
  let (t, _) = funcEnv Map.! id
  sym <- newLocalSym
  let assStr  | t == VoidT = ""
              | otherwise = (genLocSymbol sym) ++ " = "
  let call = DList.singleton $ assStr ++ "call " ++ (genType t) ++ 
              " @" ++ (genIdent id) ++ "(" ++ (genArgs vals) ++ ")"
  return (DList.concat [code, call], (VLocal (T t, sym)))

genENewArr :: Type -> Expr -> CM (Code, Val)
genENewArr t e = do
  (code, v) <- genExpr e
  check <- genCheckAgainstVal v "sle" (VConst (CInt 0))
  allocaSym <- newLocalSym
  let alloca = DList.singleton $ (genLocSymbol allocaSym) ++ " = alloca " ++ 
              (genType t) ++ ", " ++ (genTypedVal v)
  let allocaVal = VLocal (Ref t, allocaSym)
  bitcastSym <- newLocalSym
  let bitcast = DList.singleton $ (genLocSymbol bitcastSym) ++ " = bitcast " ++
                (genTypedVal allocaVal) ++ " to " ++ (genType PtrT)
  let bitcastVal = VLocal (T PtrT, bitcastSym)
  sizeOfTypeSym <- newLocalSym
  let sizeOfType = DList.singleton $ (genLocSymbol sizeOfTypeSym) ++ 
                  " = getelementptr " ++ (genType t) ++ ", " ++ 
                  (genVarType (Ref t)) ++ " null, i32 1"
  sizeOfTypeSym' <- newLocalSym
  let sizeOfType' = DList.singleton $ (genLocSymbol sizeOfTypeSym') ++ 
                    " = ptrtoint " ++ (genVarType (Ref t)) ++ " " ++ 
                    (genLocSymbol sizeOfTypeSym) ++ " to " ++ (genType IntT)
  mulSym <- newLocalSym
  let mul = DList.singleton $ (genLocSymbol mulSym) ++ " = mul " ++ 
            (genTypedVal v) ++ ", " ++ (genLocSymbol sizeOfTypeSym')
  let mulVal = VLocal (T IntT, mulSym)
  let memset = DList.singleton $ "call " ++ (genType PtrT) ++" @memset(" ++ 
              (genTypedVal bitcastVal) ++ ", " ++ (genDefaultValue IntT) ++
              ", " ++ (genTypedVal mulVal) ++ ")"
  sizeOfArrSym <- newLocalSym
  let sizeOfArr = DList.singleton $ (genLocSymbol sizeOfArrSym) ++ 
                  " = getelementptr %array, %array* null, i32 1"
  sizeOfArrSym' <- newLocalSym
  let sizeOfArr' = DList.singleton $ (genLocSymbol sizeOfArrSym') ++ 
                  " = ptrtoint %array* " ++ (genLocSymbol sizeOfArrSym) ++ 
                  " to " ++ (genType IntT)
  let arrSize = VLocal (T IntT, sizeOfArrSym')
  arrMallocSym <- newLocalSym
  let arrMalloc = DList.singleton $ (genLocSymbol arrMallocSym) ++ 
                  " = call " ++ (genType PtrT) ++ " @malloc(" ++ 
                  (genTypedVal arrSize) ++ ")"
  arrBitcastSym <- newLocalSym
  let arrBitcast = DList.singleton $ (genLocSymbol arrBitcastSym) ++ 
                  " = bitcast " ++ (genType PtrT) ++ " " ++ 
                  (genLocSymbol arrMallocSym) ++ " to %array*"
  let arrCastedVal = VLocal (T (ArrayT t), arrBitcastSym)
  arrPtrSym <- newLocalSym
  let gepArrPtr = DList.singleton $ (genLocSymbol arrPtrSym) ++ 
                  " = getelementptr %array, " ++ 
                  (genTypedVal arrCastedVal) ++ ", i32 0, i32 0"
  let storeArrPtr = DList.singleton $ "store " ++ 
                    (genTypedVal bitcastVal) ++ ", " ++ 
                    (genVarType (Ref PtrT)) ++ " " ++ (genLocSymbol arrPtrSym)
  lengthPtrSym <- newLocalSym
  let gepLengthPtr = DList.singleton $ (genLocSymbol lengthPtrSym) ++ 
                  " = getelementptr %array, " ++
                  (genTypedVal arrCastedVal) ++ ", i32 0, i32 1"
  let storeLengthPtr = DList.singleton $ "store " ++ (genTypedVal v) ++
                    ", i32* " ++ (genLocSymbol lengthPtrSym)
  return (DList.concat [
    code, check, alloca, bitcast, sizeOfType, sizeOfType', mul, 
    memset, sizeOfArr, sizeOfArr', arrMalloc, arrBitcast, gepArrPtr, 
    storeArrPtr, gepLengthPtr, storeLengthPtr
    ], arrCastedVal)

genArrLength :: Expr -> CM (Code, Val)
genArrLength e  = do
  (code, v) <- genExpr e
  lengthPtrSym <- newLocalSym
  let gepLengthPtr = DList.singleton $ (genLocSymbol lengthPtrSym) ++ 
                  " = getelementptr %array, " ++ (genTypedVal v) ++ 
                  ", i32 0, i32 1"
  let lengthPtrVal = VLocal (Ref IntT, lengthPtrSym)
  lengthSym <- newLocalSym
  let load = DList.singleton $ (genLocSymbol lengthSym) ++ " = load " ++
            (genType IntT) ++ ", " ++ (genTypedVal lengthPtrVal)
  return (DList.concat [code, gepLengthPtr, load], VLocal (T IntT, lengthSym))

genBinaryOp :: Type -> Expr -> Expr -> String -> CM (Code, Val)
genBinaryOp t e1 e2 instr = do
  (c1, v1) <- genExpr e1
  (c2, v2) <- genExpr e2
  sym <- newLocalSym
  let code = DList.singleton $ (genLocSymbol sym) ++ " = " ++ instr ++ " " ++ 
            (genTypedVal v1) ++ ", " ++ (genVal v2)
  return (DList.concat [c1, c2, code], (VLocal (T t, sym)))

genCheckAgainstVal :: Val -> String -> Val -> CM Code
genCheckAgainstVal v s vCmp = do
  icmpSym <- newLocalSym
  let icmp = DList.singleton $ (genLocSymbol icmpSym) ++ " = icmp " ++ 
            s ++ " " ++ (genTypedVal v) ++ ", " ++ (genVal vCmp)
  lError <- newLabel
  lOk <- newLabel
  let br = DList.singleton $ "br " ++ (genType BoolT) ++ " " ++ 
          (genLocSymbol icmpSym) ++ ", " ++ (genTypedLabel lError) ++
          ", " ++ (genTypedLabel lOk)
  let label1 = DList.singleton $ genLabel lError
  let error = DList.singleton $ "call void @error()"
  let br2 = DList.singleton $ "br " ++ (genTypedLabel lOk)
  let label2 = DList.singleton $ genLabel lOk
  return $ DList.concat [icmp, br, label1, error, br2, label2]

genDivOp :: Expr -> Expr -> String -> CM (Code, Val)
genDivOp e1 e2 instr = do
  (c1, v1) <- genExpr e1
  (c2, v2) <- genExpr e2
  check <- genCheckAgainstVal v2 "eq" (VConst (CInt 0))
  sym <- newLocalSym
  let code = DList.singleton $ (genLocSymbol sym) ++ " = " ++ instr ++ " " ++ 
            (genTypedVal v1) ++ ", " ++ (genVal v2)
  return (DList.concat [c1, c2, check, code], 
          (VLocal (T IntT, sym)))

genAddOp :: Expr -> AddOp -> Expr -> CM (Code, Val)
genAddOp e1 op e2 = case op of
  OMinus _ -> genBinaryOp IntT e1 e2 "sub"
  OPlus _ -> do
    (_, v) <- genExpr e1
    case v of
      VLocal (T StringT, _) -> genEApp (Ident "concatStrings") [e1, e2]
      _ -> genBinaryOp IntT e1 e2 "add"

genMulOp :: Expr -> MulOp -> Expr -> CM (Code, Val)
genMulOp e1 op e2 = case op of
  OTimes _ -> genBinaryOp IntT e1 e2 "mul"
  ODiv _ -> genDivOp e1 e2 "sdiv"
  OMod _ -> genDivOp e1 e2 "srem"

genRelOp :: Expr -> RelOp -> Expr -> CM (Code, Val)
genRelOp e1 op e2 = case op of
  OLt _ -> genBinaryOp BoolT e1 e2 "icmp slt"
  OLeq _ -> genBinaryOp BoolT e1 e2 "icmp sle"
  OGt _ -> genBinaryOp BoolT e1 e2 "icmp sgt"
  OGeq _ -> genBinaryOp BoolT e1 e2 "icmp sge"
  OEq _ -> genBinaryOp BoolT e1 e2 "icmp eq"
  ONeq _ -> genBinaryOp BoolT e1 e2 "icmp ne"

genBoolOp :: Code -> Symbol -> Symbol -> Symbol -> CM (Code, Val)
genBoolOp code lTrue lFalse lEnd = do
  let br3 = DList.singleton $ "br " ++ (genTypedLabel lEnd)
  let label3 = DList.singleton $ genLabel lEnd
  sym <- newLocalSym
  let phi = DList.singleton $ (genLocSymbol sym) ++ " = phi " ++ 
            (genType BoolT) ++ " [" ++ (genVal $ VConst (CBool True)) ++ ", " ++ 
            (genArgLabel lTrue) ++ "], [" ++ (genVal $ VConst (CBool False)) ++ 
            ", " ++ (genArgLabel lFalse) ++ "]"
  return (DList.concat [code, br3, label3, phi], (VLocal (T BoolT, sym)))

genEAnd :: Expr -> Expr -> CM (Code, Val)
genEAnd e1 e2 = do
  (c1, v1) <- genExpr e1
  lSecond <- newLabel
  (c2, v2) <- genExpr e2
  lAfterSecond <- newLabel
  lFalse <- newLabel
  lEnd <- newLabel
  let br1 = DList.singleton $ "br " ++ (genTypedVal v1) ++ ", " ++ 
          (genTypedLabel lSecond) ++ ", " ++ (genTypedLabel lFalse)
  let label1 = DList.singleton $ genLabel lSecond
  let br2 = DList.singleton $ "br " ++ (genTypedLabel lAfterSecond)
  let label2 = DList.singleton $ genLabel lAfterSecond
  let br3 = DList.singleton $ "br " ++ (genTypedVal v2) ++ ", " ++ 
          (genTypedLabel lEnd) ++ ", " ++ (genTypedLabel lFalse)
  let label3 = DList.singleton $ genLabel lFalse
  let code = DList.concat [c1, br1, label1, c2, br2, label2, br3, label3]
  genBoolOp code lAfterSecond lFalse lEnd

genEOr :: Expr -> Expr -> CM (Code, Val)
genEOr e1 e2 = do
  (c1, v1) <- genExpr e1
  lSecond <- newLabel
  (c2, v2) <- genExpr e2
  lAfterSecond <- newLabel
  lTrue <- newLabel
  lEnd <- newLabel
  let br1 = DList.singleton $ "br " ++ (genTypedVal v1) ++ ", " ++ 
          (genTypedLabel lTrue) ++ ", " ++ (genTypedLabel lSecond)
  let label1 = DList.singleton $ genLabel lSecond
  let br2 = DList.singleton $ "br " ++ (genTypedLabel lAfterSecond)
  let label2 = DList.singleton $ genLabel lAfterSecond
  let br3 = DList.singleton $ "br " ++ (genTypedVal v2) ++ ", " ++ 
          (genTypedLabel lTrue) ++ ", " ++ (genTypedLabel lEnd)
  let label3 = DList.singleton $ genLabel lTrue
  let code = DList.concat [c1, br1, label1, c2, br2, label2, br3, label3]
  genBoolOp code lTrue lAfterSecond lEnd

genExpr :: Expr -> CM (Code, Val)
genExpr e = case e of
  ELitInt _ i -> return (DList.empty, VConst (CInt i))
  ELitTrue _ -> return (DList.empty, VConst (CBool True))
  ELitFalse _ -> return (DList.empty, VConst (CBool False))
  EString _ s -> genEString s
  ELVal _ lv -> genELVal lv
  EApp _ id es -> genEApp id es
  ENewArr _ tt e -> genENewArr (convType tt) e
  EAttr _ e _ -> genArrLength e
  ENeg pos e -> genAddOp (ELitInt pos 0) (OMinus pos) e
  ENot pos e -> genRelOp (ELitFalse pos) (OEq pos) e
  EMul _ e1 op e2 -> genMulOp e1 op e2
  EAdd _ e1 op e2 -> genAddOp e1 op e2
  ERel _ e1 op e2 -> genRelOp e1 op e2
  EAnd _ e1 e2 -> genEAnd e1 e2
  EOr _ e1 e2 -> genEOr e1 e2
  -- _ -> return (DList.empty, VConst (CBool False))

genExprs :: [Expr]-> (Code, [Val]) -> CM (Code, [Val])
genExprs [] (codeAcc, valAcc) = return (codeAcc, reverse valAcc)
genExprs (e:es) (codeAcc, valAcc) = do
  (code, val) <- genExpr e
  genExprs es ((DList.append codeAcc code), (val:valAcc))
