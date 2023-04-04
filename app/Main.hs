module Main (main) where

import qualified System.Environment

main :: IO ()
main = do
  args <- System.Environment.getArgs
  print args
