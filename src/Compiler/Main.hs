module Main where

import System.IO
import System.Process         (callProcess)

import Frontend.Utils
import Frontend.Parser        (parse)
import Frontend.Typechecker   (typecheck)

main :: IO ()
main = do
  (file, filePathNoExt) <- readSource
  progr <- parse file
  typecheck progr
  -- code <- compile progr
  -- writeFile (filePathNoExt ++ ".ll") code
  -- out <- callProcess "llvm-as" [
  --   "-o", filePathNoExt ++ ".bc", 
  --   filePathNoExt ++ ".ll"
  --   ]
  printSuccess
  return ()
