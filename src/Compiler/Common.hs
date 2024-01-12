module Common where

import System.IO
import System.Exit (exitFailure)

import Data.Map (Map)

import Latte.Abs
import Latte.Print (printTree)

type Pos = BNFC'Position

type FuncType = (Type, [Type])
type FuncEnv = Map Ident FuncType
type AttrEnv = Map Ident (Int, Type)
type MethodEnv = Map Ident (Int, Ident, FuncType)
type Super = Maybe Ident
type ClassEnv = Map Ident (AttrEnv, MethodEnv, Super)

data Type = IntT | StringT | BoolT | VoidT | ArrayT Type | PtrT | ClassT Ident
  deriving Eq

data Const = CInt Integer | CBool Bool | CNull

convType :: TType -> Type
convType (TInt _) = IntT
convType (TBool _) = BoolT
convType (TString _) = StringT
convType (TVoid _) = VoidT
convType (TArray _ tt) = ArrayT (convType tt)
convType (TClass _ id) = ClassT id

typeOfArrayElem :: Type -> Type
typeOfArrayElem (ArrayT t) = t

isArrayType :: Type -> Bool
isArrayType (ArrayT _) = True
isArrayType _ = False

isClassType :: Type -> Bool
isClassType (ClassT _) = True
isClassType _ = False

classIdent :: Type -> Ident
classIdent (ClassT id) = id

restrictName :: String -> String
restrictName s = "_" ++ s

posStr :: Pos -> String
posStr Nothing = "in unknown position:\n"
posStr (Just (l, c)) = "at line " ++ show l ++ ", column " ++ show c ++ ":\n"

tryEval :: Expr -> Maybe Const
tryEval e = case e of
  ELitInt _ i -> Just $ CInt i
  ELitTrue _ -> Just $ CBool True
  ELitFalse _ -> Just $ CBool False
  ENot _ e -> case tryEval e of
    Just (CBool b) -> Just $ CBool (not b)
    Nothing -> Nothing
  _ -> Nothing

printSuccess :: IO ()
printSuccess = hPutStrLn stderr "OK"

printError :: String -> IO a
printError err = do
  hPutStrLn stderr "ERROR"
  hPutStrLn stderr err
  exitFailure
