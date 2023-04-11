{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Ipe.Cli (Options (..))
import Ipe.Settings (appSettings)
import qualified Iris
import qualified System.Console.Pretty as Pretty

newtype App a = App
  { unApp :: Iris.CliApp Options () a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Iris.CliEnv Options ())
    )

app :: App ()
app = do
  -- 1. Get parsed 'Options' from the environment
  Options {..} <- Iris.asksCliEnv Iris.cliEnvCmd

  -- 2. Read the file
  fileContent <- getFileContent optionsFile

  -- 3. Find all lines with numbers
  let searchResult = search optionsSearch fileContent

  -- 4. Output the result
  output searchResult

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app

-- Business logic

getFileContent :: FilePath -> App Text
getFileContent = liftIO . Text.readFile

search :: Text -> Text -> [(Int, Text)]
search str =
  filter (\(_i, line) -> str `Text.isInfixOf` line)
    . zip [1 ..]
    . Text.lines

output :: [(Int, Text)] -> App ()
output = traverse_ outputLine
  where
    outputLine :: (Int, Text) -> App ()
    outputLine (i, line) = do
      outputLineNumber i
      liftIO $ Text.putStrLn line

    outputLineNumber :: Int -> App ()
    outputLineNumber i =
      Iris.putStderrColoured
        (Pretty.color Pretty.Yellow . Pretty.style Pretty.Bold)
        (Text.pack (show i) <> ": ")
