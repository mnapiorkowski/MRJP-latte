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
      let newVar = (Arr ((1 + length s), CharT), g)
      setGlobal (Ident s) newVar
      return newVar
  sym' <- newLocalSym
  let bitcast = (genLocSymbol sym') ++ " = bitcast " ++ (genVarType t) ++ 
                "* " ++ (genGlobSymbol sym) ++ " to " ++ (genType StringT)
  return (DList.singleton bitcast, (VLocal (T StringT, sym')))

genELVal :: LVal -> CM (Code, Val)
genELVal lv = case lv of
  LVar _ id -> do
    var@(t, sym) <- getLocal id
    case sym of
      NumSym _ -> do
        let t' = dereference t
        sym' <- newLocalSym
        let load = DList.singleton $ (genLocSymbol sym') ++ " = load " ++ 
                  (genVarType t') ++ ", " ++ genTypedVal (VLocal var)
        return (load, (VLocal (t', sym')))
      StrSym str -> return (DList.empty, (VLocal var))
  -- LArr _ id e

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
  return (DList.concat [code, call], (VLocal ((T t), sym)))

genBinaryOp :: Type -> Expr -> Expr -> String -> CM (Code, Val)
genBinaryOp t e1 e2 instr = do
  (c1, v1) <- genExpr e1
  (c2, v2) <- genExpr e2
  sym <- newLocalSym
  let code = DList.singleton $ (genLocSymbol sym) ++ " = " ++ instr ++ " " ++ 
            (genTypedVal v1) ++ ", " ++ (genVal v2)
  return (DList.concat [c1, c2, code], (VLocal ((T t), sym)))

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
  ODiv _ -> genBinaryOp IntT e1 e2 "sdiv"
  OMod _ -> genBinaryOp IntT e1 e2 "srem"

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
  return (DList.concat [code, br3, label3, phi], (VLocal ((T BoolT), sym)))

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
  -- ENewArr pos tt e -> typeOfNewArr pos tt e
  -- EAttr pos e id -> typeOfAttr pos e id
  ENeg pos e -> genAddOp (ELitInt pos 0) (OMinus pos) e
  ENot pos e -> genRelOp (ELitFalse pos) (OEq pos) e
  EMul _ e1 op e2 -> genMulOp e1 op e2
  EAdd _ e1 op e2 -> genAddOp e1 op e2
  ERel _ e1 op e2 -> genRelOp e1 op e2
  EAnd _ e1 e2 -> genEAnd e1 e2
  EOr _ e1 e2 -> genEOr e1 e2
  _ -> return (DList.empty, VConst (CBool False))

genExprs :: [Expr]-> (Code, [Val]) -> CM (Code, [Val])
genExprs [] (codeAcc, valAcc) = return (codeAcc, reverse valAcc)
genExprs (e:es) (codeAcc, valAcc) = do
  (code, val) <- genExpr e
  genExprs es ((DList.append codeAcc code), (val:valAcc))
