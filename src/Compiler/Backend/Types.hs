module Backend.Types where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.Map (Map)
import Data.DList (DList)

import Latte.Abs (Ident, Expr)

import Common

type Code = DList String

type Loc = Int
data Register = Reg Loc | RegArg String
data RegType = T Type | Ref Type
type Var = (RegType, Register)
data Val = VInt Integer | VFalse | VTrue | VVar Var

type CEnv = FuncEnv
type CState = (Map Ident Var, Loc)
type Result = Except String
type CM a = StateT CState (ReaderT CEnv Result) a
