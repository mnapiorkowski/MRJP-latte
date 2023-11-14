module Backend.Types where

import Control.Monad.Except
import Control.Monad.State

import Data.Map (Map)
import Data.DList (DList)

-- import Data.Set (Set)
-- import qualified Data.Set as Set

import Latte.Abs (Ident, Expr, BNFC'Position)

type Pos = BNFC'Position

type Code = DList String

type Loc = Int
type CState = (Map Ident Loc, Loc)

type Result = Except String
type CM a = StateT CState (Result) a
