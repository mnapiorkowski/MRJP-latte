module Backend.Types where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.Map (Map)
import Data.DList (DList)

import Latte.Abs (Ident, Expr)

import Common

type Code = DList String

data Symbol = NumSym Int | StrSym String
data VarType = T Type | Ref Type | Arr (Int, Type)
type Var = (VarType, Symbol)
data Val = VInt Integer | VFalse | VTrue | VLocal Var | VGlobal Var

type CEnv = FuncEnv

type Locals = Map Ident Var
type Globals = Map Ident Var
type CState = (Locals, Globals, Int, Int)

type Result = Except String
type CM a = StateT CState (ReaderT CEnv Result) a
