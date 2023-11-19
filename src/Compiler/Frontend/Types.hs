module Frontend.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Latte.Abs (Ident, Expr)

import Common

data Val = IntV Integer | StringV String | BoolV Bool

type VarEnv = Map Ident Type
type VarsInBlock = Set Ident
type Env = (VarEnv, FuncEnv, VarsInBlock)

type CurrentFunc = Ident
type Context = (CurrentFunc)

type Result = Except String
type TM a = StateT Context (ReaderT Env Result) a
