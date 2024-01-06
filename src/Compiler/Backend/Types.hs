module Backend.Types where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.Map (Map)
import Data.DList (DList)

import Latte.Abs (Ident, Expr)

import Common

type Code = DList String
type HasRet = Bool
type RetType = Type

data Symbol = NumSym Int | StrSym String
data VarType = T Type | Ref Type | StringLit Int
type Var = (VarType, Symbol)
data Val = VConst Const | VLocal Var | VGlobal Var

type CEnv = (FuncEnv, ClassEnv)

type Locals = Map Ident Var
type Globals = Map Ident Var
type Counters = (Int, Int, Int) -- locals, globals, labels
type Context = (RetType)
type CState = (Locals, Globals, Counters, Context)

type Result = Except String
type CM a = StateT CState (ReaderT CEnv Result) a
