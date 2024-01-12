module Main where

import System.IO
import System.Process (callProcess)
import System.Environment (getArgs)
import System.FilePath.Posix (splitExtension)

import Control.Exception (try, IOException)

import Common
import Frontend.Parser (parse)
import Frontend.Typechecker.Program (typecheck)
import Backend.Generator.Program (compile)

tryReadFile :: String -> IO String
tryReadFile filePath = do
  res <- try $ readFile $ filePath :: IO (Either IOException String)
  case res of
    Left _ -> printError $ "could not read file " ++ filePath
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

main :: IO ()
main = do
  (file, filePathNoExt) <- readSource
  progr <- parse file
  env <- typecheck progr
  code <- compile progr env
  writeFile (filePathNoExt ++ ".ll") code
  callProcess "llvm-as" [
    "-o", filePathNoExt ++ "_temp.bc", 
    filePathNoExt ++ ".ll"
    ]
  callProcess "llvm-link" [
    "-o", filePathNoExt ++ ".bc",
    filePathNoExt ++ "_temp.bc",
    "lib/runtime.bc"
    ]
  callProcess "rm" [filePathNoExt ++ "_temp.bc"]
  printSuccess
  return ()
