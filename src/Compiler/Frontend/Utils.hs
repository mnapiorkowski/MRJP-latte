module Frontend.Utils where

import System.IO
import System.Environment     (getArgs)
import System.FilePath.Posix  (splitExtension)
import System.Exit            (exitFailure)

import qualified Control.Monad.Except as E ( throwError )
import Control.Monad.Reader (lift)
import Control.Exception

import Latte.Abs
import Latte.Print (printTree)

import Frontend.Types

showType :: Type -> String
showType IntT = "int"
showType BoolT = "bool"
showType StringT = "string"
showType VoidT = "void"
showType (ArrayT t) = (showType t) ++ "[]"

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

throwE :: Pos -> String -> TM a
throwE pos s = lift $ E.throwError (posStr pos ++ s)

printSuccess :: IO ()
printSuccess = hPutStrLn stderr "OK"

printError :: String -> IO a
printError err = do
  hPutStrLn stderr "ERROR"
  hPutStrLn stderr err
  exitFailure

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
      res <- try $ readFile $ filePath :: IO (Either IOException String)
      case res of
        Left _ -> printError ("could not read file " ++ filePath)
        Right file -> return (file, filePathNoExt)
