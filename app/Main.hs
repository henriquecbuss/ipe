module Main (main) where

import qualified Ipe
import qualified System.Environment

main :: IO ()
main = do
  args <- System.Environment.getArgs
  print $ Ipe.parseAssign $ head args
