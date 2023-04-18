{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Ipe.Cli (Options (..))
import qualified Ipe.Emitter.Module
import qualified Ipe.Grammar
import qualified Ipe.Parser
import Ipe.Settings (appSettings)
import qualified Ipe.Transformer.Module
import qualified Ipe.TypeChecker
import qualified Iris
import Prettyprinter.Render.Text (hPutDoc)
import qualified System.Directory
import qualified System.FilePath
import System.IO (IOMode (WriteMode), withFile)

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
  possiblyParsedModule <- liftIO $ Ipe.Parser.parseFile entrypoint

  case possiblyParsedModule of
    Left err -> liftIO $ putStr err
    Right parsedModule -> do
      let transformedModule = Ipe.Transformer.Module.apply parsedModule

      case Ipe.TypeChecker.run [] transformedModule of
        Left err -> liftIO $ putStr err
        Right typeChecked -> do
          let modulePath = Ipe.Grammar.moduleDefinitionPath $ Ipe.Grammar.moduleDefinition parsedModule
          let moduleName = Ipe.Grammar.moduleDefinitionName $ Ipe.Grammar.moduleDefinition parsedModule

          liftIO $ print parsedModule
          liftIO $ print typeChecked
          currentDirectory <- liftIO System.Directory.getCurrentDirectory
          absoluteOutputDir <- liftIO $ System.Directory.makeAbsolute $ Maybe.fromMaybe currentDirectory outputDir

          let doc = Ipe.Emitter.Module.emit transformedModule
          let outPath =
                System.FilePath.joinPath
                  [ absoluteOutputDir,
                    System.FilePath.joinPath (map T.unpack modulePath),
                    T.unpack moduleName ++ ".ipe.js"
                  ]

          liftIO $ System.Directory.createDirectoryIfMissing True $ System.FilePath.takeDirectory outPath

          liftIO $ withFile outPath WriteMode $ \h -> do
            hPutDoc h doc

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
