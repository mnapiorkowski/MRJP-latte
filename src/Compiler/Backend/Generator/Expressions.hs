module Backend.Generator.Expressions where

import qualified Data.Map as Map
import qualified Data.DList as DList
import Data.List (intercalate)

import Control.Monad.Reader

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils

genCastToSuper :: Val -> Type -> CM (Code, Val)
genCastToSuper v superT = do
  (subT, superVar) <- case valType v of
    T t -> return (t, T superT)
    Ref t -> return (t, Ref superT)
  compatible <- areTypesCompatible superT subT
  if (subT == superT) || (not compatible)
    then return (DList.empty, v)
  else do
    sym <- newLocalSym
    let bitcast = DList.singleton $ (genLocSymbol sym) ++ " = bitcast " ++
                  genTypedVal v ++ " to " ++ genVarType superVar
    let castedVal = VLocal (superVar, sym)
    return (bitcast, castedVal)

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
            genArrayType ++ ", " ++ (genTypedVal arr) ++ ", " ++ 
            (genType IntT) ++ " 0, " ++ (genType IntT) ++ " 1"
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
            genArrayType ++ ", " ++ (genTypedVal arr) ++ ", " ++ 
            (genType IntT) ++ " 0, " ++ (genType IntT) ++ " 0"
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
  local <- tryGetLocal id
  case local of
    Just var@(t, _) -> do
      let t' = dereference t
      sym' <- newLocalSym
      let load = DList.singleton $ (genLocSymbol sym') ++ " = load " ++ 
                (genVarType t') ++ ", " ++ genTypedVal (VLocal var)
      return (load, (VLocal (t', sym')))
    Nothing -> genELVal (LAttr Nothing (ESelf Nothing) id)

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
  LAttr _ e id -> do
    (code, v) <- genExpr e
    let t = convVarType $ valType v
    if (isArrayType t) && (id == Ident "length")
      then do
        (lengthCode, vLength) <- genGetArrayLength v
        return (DList.concat [code, lengthCode], vLength)
    else do
      attributes <- getAttributes (classIdent t)
      let (num, attrT) = attributes Map.! id
      ptrSym <- newLocalSym
      let ptr = VLocal (Ref attrT, ptrSym)
      let gep = DList.singleton $ (genLocSymbol ptrSym) ++ 
                " = getelementptr " ++ (genClassType (classIdent t)) ++ ", " ++ 
                (genTypedVal v) ++ ", " ++ (genType IntT) ++ 
                " 0, " ++ (genType IntT) ++ " " ++ show num
      attrSym <- newLocalSym
      let attr = VLocal (T attrT, attrSym)
      let load = DList.singleton $ (genLocSymbol attrSym) ++ 
                " = load " ++ (genType attrT) ++ ", " ++ (genTypedVal ptr)
      return (DList.concat [code, gep, load], attr)

genArg :: Val -> Type -> CM (Code, String)
genArg v t = do
  (cast, v') <- genCastToSuper v t
  return (cast, genTypedVal v')

genArgs :: [Val] -> [Type] -> Code -> [String] -> CM (Code, [String])
genArgs [] _ codeAcc argsAcc = return (codeAcc, reverse argsAcc)
genArgs (v:vs) (t:ts) codeAcc argsAcc = do
  (code, arg) <- genArg v t
  genArgs vs ts (DList.append codeAcc code) (arg:argsAcc)

genECall :: Ident -> [Expr] -> CM (Code, Val)
genECall id es = do
  (funcEnv, _) <- ask
  if Map.member id funcEnv
    then do
      (code, vals) <- genExprs es (DList.empty, [])
      let (t, paramTs) = funcEnv Map.! id
      (casts, args) <- genArgs vals paramTs DList.empty []
      let argsStr = intercalate ", " args
      sym <- newLocalSym
      let assStr  | t == VoidT = ""
                  | otherwise = (genLocSymbol sym) ++ " = "
      let call = DList.singleton $ assStr ++ "call " ++ (genType t) ++ 
                  " @" ++ (genIdent id) ++ "(" ++ argsStr ++ ")"
      return (DList.concat [code, casts, call], (VLocal (T t, sym)))
  else genEMetCall (ESelf Nothing) id es

findClassWithMethod :: Ident -> Ident -> CM Ident
findClassWithMethod id classId = do
  methods <- getMethods classId
  if Map.member id methods
    then return classId
  else do
    super <- getSuper classId
    case super of
      Just superId -> findClassWithMethod id superId

genEMetCall :: Expr -> Ident -> [Expr] -> CM (Code, Val)
genEMetCall e id es = do
  (code, vals) <- genExprs (e:es) (DList.empty, [])
  let classType = convVarType $ valType (head vals)
  classId <- findClassWithMethod id (classIdent classType)
  methods <- getMethods classId
  let (t, paramTs) = methods Map.! id
  (casts, args) <- genArgs vals ((ClassT classId):paramTs) DList.empty []
  let argsStr = intercalate ", " args
  sym <- newLocalSym
  let assStr  | t == VoidT = ""
              | otherwise = (genLocSymbol sym) ++ " = "
  let call = DList.singleton $ assStr ++ "call " ++ (genType t) ++ 
              " @" ++ (restrictName $ genIdent classId) ++ "_" ++ 
              genIdent id ++ "(" ++ argsStr ++ ")"
  return (DList.concat [code, casts, call], (VLocal (T t, sym)))

genAttrInit :: Val -> (Int, Type) -> CM Code
genAttrInit v (num, t) = do
  let classT = genClassType $ classIdent $ convVarType $ valType v
  ptrSym <- newLocalSym
  let gep = DList.singleton $ (genLocSymbol ptrSym) ++ 
            " = getelementptr " ++ classT ++ ", " ++ 
            (genTypedVal v) ++ ", " ++ (genType IntT) ++ 
            " 0, " ++ (genType IntT) ++ " " ++ show num
  let store = DList.singleton $ "store " ++ 
              (genTypedDefaultVal t) ++ ", " ++ 
              (genVarType (Ref t)) ++ " " ++ (genLocSymbol ptrSym)
  return $ DList.concat [gep, store]

genAttrInits :: Val -> [(Int, Type)] -> Code -> CM Code
genAttrInits _ [] acc = return acc
genAttrInits v (a:as) acc = do
  code <- genAttrInit v a
  genAttrInits v as (DList.append acc code)

genENewObj :: Type -> CM (Code, Val)
genENewObj t@(ClassT id) = do
  mallocSym <- newLocalSym
  let mallocVal = VLocal (T t, mallocSym)
  let malloc = DList.singleton $ (genLocSymbol mallocSym) ++ " = call " ++ 
              genType t ++ " @" ++ (restrictName $ genIdent id) ++ "_" ++
              restrictName "malloc" ++ "()"
  attributes <- getAttributes id
  let attrList = Map.elems attributes
  inits <- genAttrInits mallocVal attrList DList.empty
  return (DList.concat [malloc, inits], mallocVal)

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
                  (genVarType (Ref t)) ++ " " ++ genVal (VConst CNull) ++ 
                  ", " ++ (genType IntT) ++ " 1"
  sizeOfTypeSym' <- newLocalSym
  let sizeOfType' = DList.singleton $ (genLocSymbol sizeOfTypeSym') ++ 
                    " = ptrtoint " ++ (genVarType (Ref t)) ++ " " ++ 
                    (genLocSymbol sizeOfTypeSym) ++ " to " ++ (genType IntT)
  let sizeOfTypeVal = VLocal (T IntT, sizeOfTypeSym')
  let memset = DList.singleton $ "call " ++ (genType VoidT) ++ 
              " @" ++ restrictName "clearNElems" ++ "(" ++ 
              (genTypedVal bitcastVal) ++ ", " ++ (genTypedVal v) ++ 
              ", " ++ (genTypedVal sizeOfTypeVal) ++ ")"
  arrMallocSym <- newLocalSym
  let arrMalloc = DList.singleton $ (genLocSymbol arrMallocSym) ++ 
                  " = call " ++ (genType (ArrayT t)) ++ " @" ++ 
                  restrictName (restrictName "array" ++ "_" ++ 
                  restrictName "malloc") ++ "()"
  let arrVal = VLocal (T (ArrayT t), arrMallocSym)
  arrPtrSym <- newLocalSym
  let gepArrPtr = DList.singleton $ (genLocSymbol arrPtrSym) ++ 
                  " = getelementptr " ++ genArrayType ++ ", " ++ 
                  (genTypedVal arrVal) ++ ", " ++ (genType IntT) ++ 
                  " 0, " ++ (genType IntT) ++ " 0"
  let storeArrPtr = DList.singleton $ "store " ++ 
                    (genTypedVal bitcastVal) ++ ", " ++ 
                    (genVarType (Ref PtrT)) ++ " " ++ (genLocSymbol arrPtrSym)
  lengthPtrSym <- newLocalSym
  let gepLengthPtr = DList.singleton $ (genLocSymbol lengthPtrSym) ++ 
                  " = getelementptr " ++ genArrayType ++ ", " ++
                  (genTypedVal arrVal) ++ ", " ++ (genType IntT) ++ 
                  " 0, " ++ (genType IntT) ++ " 1"
  let storeLengthPtr = DList.singleton $ "store " ++ (genTypedVal v) ++
                    ", " ++ (genVarType (Ref IntT)) ++ " " ++ 
                    (genLocSymbol lengthPtrSym)
  return (DList.concat [
    code, check, alloca, bitcast, sizeOfType, sizeOfType', memset, 
    arrMalloc, gepArrPtr, storeArrPtr, gepLengthPtr, storeLengthPtr
    ], arrVal)

genArrLength :: Expr -> CM (Code, Val)
genArrLength e  = do
  (code, v) <- genExpr e
  lengthPtrSym <- newLocalSym
  let gepLengthPtr = DList.singleton $ (genLocSymbol lengthPtrSym) ++ 
                  " = getelementptr " ++ genArrayType ++ ", " ++ 
                  (genTypedVal v) ++ ", " ++ (genType IntT) ++ " 0, " ++ 
                  (genType IntT) ++ " 1"
  let lengthPtrVal = VLocal (Ref IntT, lengthPtrSym)
  lengthSym <- newLocalSym
  let load = DList.singleton $ (genLocSymbol lengthSym) ++ " = load " ++
            (genType IntT) ++ ", " ++ (genTypedVal lengthPtrVal)
  return (DList.concat [code, gepLengthPtr, load], VLocal (T IntT, lengthSym))

genECast :: Type -> Expr -> CM (Code, Val)
genECast t e = do
  (code, v) <- genExpr e
  sym <- newLocalSym
  let vCasted = VLocal (T t, sym)
  let bitcast = DList.singleton $ (genLocSymbol sym) ++ " = bitcast " ++ 
                (genTypedVal v)  ++ " to " ++ (genType t)
  return (DList.concat [code, bitcast], vCasted)

genBinaryOp :: Type -> Expr -> Expr -> String -> CM (Code, Val)
genBinaryOp t e1 e2 instr = do
  (c1, v1) <- genExpr e1
  let t1 = convVarType $ valType v1
  (c2, v2) <- genExpr e2
  let t2 = convVarType $ valType v2
  (cast1, v1') <- genCastToSuper v1 t2
  (cast2, v2') <- genCastToSuper v2 t1
  sym <- newLocalSym
  let code = DList.singleton $ (genLocSymbol sym) ++ " = " ++ instr ++ " " ++ 
            (genTypedVal v1') ++ ", " ++ (genVal v2')
  return (DList.concat [c1, c2, cast1, cast2, code], (VLocal (T t, sym)))

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
      VLocal (T StringT, _) -> 
        genECall (Ident (restrictName "concatStrings")) [e1, e2]
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
  ENull _ -> return (DList.empty, VConst CNull)
  ELVal _ lv -> genELVal lv
  ESelf _ -> genLoadVar (Ident $ restrictName "this")
  ECall _ id es -> genECall id es
  EMetCall _ e id es -> genEMetCall e id es
  ENewObj _ tt -> genENewObj (convType tt)
  ENewArr _ tt e -> genENewArr (convType tt) e
  ECast _ tt e -> genECast (convType tt) e
  ECastClass _ (ELVal _ (LVar _ id)) e -> genECast (ClassT id) e
  ENeg _ e -> genAddOp (ELitInt Nothing 0) (OMinus Nothing) e
  ENot _ e -> genRelOp (ELitFalse Nothing) (OEq Nothing) e
  EMul _ e1 op e2 -> genMulOp e1 op e2
  EAdd _ e1 op e2 -> genAddOp e1 op e2
  ERel _ e1 op e2 -> genRelOp e1 op e2
  EAnd _ e1 e2 -> genEAnd e1 e2
  EOr _ e1 e2 -> genEOr e1 e2

genExprs :: [Expr]-> (Code, [Val]) -> CM (Code, [Val])
genExprs [] (codeAcc, valAcc) = return (codeAcc, reverse valAcc)
genExprs (e:es) (codeAcc, valAcc) = do
  (code, val) <- genExpr e
  genExprs es ((DList.append codeAcc code), (val:valAcc))
