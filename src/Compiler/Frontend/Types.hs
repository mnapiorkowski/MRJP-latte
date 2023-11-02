module Frontend.Types where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

import Latte.Abs (Ident, Expr, BNFC'Position)

type Pos = BNFC'Position

data Type = IntT | StringT | BoolT | VoidT | ArrayT Type
  deriving Eq

type VarEnv = Map Ident Type
type FuncEnv = Map Ident (Type, [Type])
type Env = (VarEnv, FuncEnv)

type Result = Except String
type TM a = ReaderT Env Result a
