module Frontend.Utils where

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader ( lift )

import Common
import Frontend.Types

showType :: Type -> String
showType IntT = "int"
showType BoolT = "bool"
showType StringT = "string"
showType VoidT = "void"
showType (ArrayT t) = (showType t) ++ "[]"

throwE :: Pos -> String -> TM a
throwE pos s = lift $ E.throwError (posStr pos ++ s)
