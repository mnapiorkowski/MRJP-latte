module Backend.Generator.Expressions where

import qualified Data.Map as Map
import qualified Data.DList as DList

import Control.Monad.Reader

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils

import Data.Typeable

genELVal :: LVal -> CM (Code, Val)
genELVal lv = case lv of
  LVar _ id -> do
    var@(t, r) <- getVar id
    case r of
      Reg _ -> do
        let t' = dereference t
        r <- newReg
        let load = DList.singleton $ (genReg r) ++ " = load " ++ 
                  (genRegType t') ++ ", " ++ genTypedVal (VVar var)
        return (load, (VVar (t', r)))
      RegArg str -> return (DList.empty, (VVar var))
  -- LArr _ id e

genEApp :: Ident -> [Expr] -> CM (Code, Val)
genEApp id es = do
  (code, vals) <- genExprs es (DList.empty, [])
  funcEnv <- ask
  let (t, _) = funcEnv Map.! id
  r <- newReg
  let assStr  | t == VoidT = ""
              | otherwise = (genReg r) ++ " = "
  let call = DList.singleton $ assStr ++ "call " ++ (genType t) ++ 
              " @" ++ (genIdent id) ++ "(" ++ (genArgs vals) ++ ")"
  return (DList.concat [code, call], (VVar ((T t), r)))

genExpr :: Expr -> CM (Code, Val)
genExpr e = case e of
  ELitInt _ i -> return (DList.empty, VInt i)
  ELitTrue _ -> return (DList.empty, VTrue)
  ELitFalse _ -> return (DList.empty, VFalse)
  -- EString _ s -> return StringT
  ELVal _ lv -> genELVal lv
  EApp _ id es -> genEApp id es
  -- ENewArr pos tt e -> typeOfNewArr pos tt e
  -- EAttr pos e id -> typeOfAttr pos e id
  -- ENeg pos e -> checkUnaryOp pos IntT e >> return IntT
  -- ENot pos e -> checkUnaryOp pos BoolT e >> return BoolT
  -- EMul pos e1 op e2 -> typeOfMulOp pos e1 op e2
  -- EAdd pos e1 op e2 -> typeOfAddOp pos e1 op e2
  -- ERel pos e1 op e2 -> typeOfRelOp pos e1 op e2
  -- EAnd pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
  -- EOr pos e1 e2 -> checkBinaryOp pos BoolT e1 e2 >> return BoolT
  _ -> return (DList.empty, VFalse)

genExprs :: [Expr]-> (Code, [Val]) -> CM (Code, [Val])
genExprs [] acc = return acc
genExprs (e:es) (codeAcc, valAcc) = do
  (code, val) <- genExpr e
  genExprs es ((DList.append codeAcc code), (val:valAcc))
