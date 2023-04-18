{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Ipe.Cli (Options (..))
import qualified Ipe.Parser
import Ipe.Settings (appSettings)
import qualified Ipe.Transformer.Module
import qualified Ipe.TypeChecker
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
  options <- Iris.asksCliEnv Iris.cliEnvCmd

  case options of
    Build {entrypoint, outputDir} ->
      execBuild entrypoint outputDir

execBuild :: FilePath -> Maybe FilePath -> App ()
execBuild entrypoint outputDir = do
  parsedModule <- liftIO $ Ipe.Parser.parseFile entrypoint

  case parsedModule of
    Left err -> liftIO $ putStr err
    Right parsedModule -> do
      case Ipe.TypeChecker.run [] $ Ipe.Transformer.Module.apply parsedModule of
        Left err -> liftIO $ putStr err
        Right typeChecked -> do
          liftIO $ print parsedModule
          liftIO $ print typeChecked

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
