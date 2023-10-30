module Frontend.Utils where

import System.IO
import System.Environment     (getArgs)
import System.FilePath.Posix  (splitExtension)
import System.Exit            (exitFailure)

import Control.Exception

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
    then printError "Usage: ./latc_llvm <source file>"
  else do
    let filePath = args !! 0
    let (filePathNoExt, ext) = splitExtension filePath
    if ext /= ".lat"
      then printError "Source file must have extension .lat"
    else do
      res <- try $ readFile $ filePath :: IO (Either IOException String)
      case res of
        Left _ -> printError ("Could not read file " ++ filePath)
        Right file -> return (file, filePathNoExt)
