module Main where

import System.IO
import System.Process                 (callProcess)

import Utils 
import Frontend.Parser                (parse)
import Frontend.Typechecker.Program   (typecheck)

import Backend.Generator.Program      (compile)

main :: IO ()
main = do
  (file, filePathNoExt) <- readSource
  progr <- parse file
  typecheck progr
  code <- compile progr
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
