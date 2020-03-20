module Main where

import Data.Aeson (encodeFile)
import Lib (buildCards)
import System.Environment (getArgs)

main :: IO ()
main = do
  (inPath:outPath:_) <- getArgs
  contents <- readFile inPath
  case buildCards contents of
    Left e   -> fail e
    Right cs -> encodeFile outPath cs
