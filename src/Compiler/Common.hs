module Common where

import System.IO
import System.Environment     (getArgs)
import System.FilePath.Posix  (splitExtension)
import System.Exit            (exitFailure)

import Control.Exception

import Data.Map (Map)

import Latte.Abs
import Latte.Print (printTree)

type Pos = BNFC'Position

type FuncEnv = Map Ident (Type, [Type])

data Type = IntT | StringT | BoolT | VoidT | ArrayT Type
  deriving Eq

convType :: TType -> Type
convType (TInt _) = IntT
convType (TBool _) = BoolT
convType (TString _) = StringT
convType (TVoid _) = VoidT
convType (TArray _ tt) = ArrayT (convType tt)

typeOfArrayElem :: Type -> Type
typeOfArrayElem (ArrayT t) = t

isArrayType :: Type -> Bool
isArrayType (ArrayT _) = True
isArrayType _ = False

posStr :: Pos -> String
posStr Nothing = "in unknown position:\n"
posStr (Just (l, c)) = "at line " ++ show l ++ ", column " ++ show c ++ ":\n"

printSuccess :: IO ()
printSuccess = hPutStrLn stderr "OK"

printError :: String -> IO a
printError err = do
  hPutStrLn stderr "ERROR"
  hPutStrLn stderr err
  exitFailure

tryReadFile :: String -> IO String
tryReadFile filePath = do
  res <- try $ readFile $ filePath :: IO (Either IOException String)
  case res of
    Left _ -> printError ("could not read file " ++ filePath)
    Right file -> return file

readSource :: IO (String, String)
readSource = do
  args <- getArgs
  if length args /= 1
    then printError "usage: ./latc_llvm <source file>"
  else do
    let filePath = args !! 0
    let (filePathNoExt, ext) = splitExtension filePath
    if ext /= ".lat"
      then printError "source file must have extension .lat"
    else do
      file <- tryReadFile filePath
      return (file, filePathNoExt)

