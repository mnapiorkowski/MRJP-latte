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

type VarEnv = Map Ident Type
type VarsInBlock = Set Ident
type Env = (VarEnv, FuncEnv, ClassEnv, VarsInBlock)

type RetType = Type
type InClass = Maybe Ident
type Context = (RetType, InClass)

type Result = Except String
type TM a = StateT Context (ReaderT Env Result) a
