{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import qualified Data.ByteString as ByteString
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Cli (Options (..))
import qualified Ipe.Emitter.Module
import qualified Ipe.Github
import qualified Ipe.Grammar
import qualified Ipe.Parser
import Ipe.Settings (appSettings)
import qualified Ipe.Transformer.Module
import qualified Ipe.TypeChecker
import qualified Iris
import Prettyprinter (Doc)
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
  rootDir <- liftIO $ System.Directory.makeAbsolute $ System.FilePath.takeDirectory entrypoint

  possiblyAllImportedModules <-
    liftIO $
      fetchAllImportedAndTransformedModules
        Map.empty
        rootDir
        []
        (T.pack $ System.FilePath.dropExtension $ System.FilePath.takeFileName entrypoint)

  case possiblyAllImportedModules of
    Left err -> liftIO $ putStr err
    Right allImportedAndTransformedModules -> do
      let result =
            buildModule
              rootDir
              allImportedAndTransformedModules
              Map.empty
              ([], T.pack $ System.FilePath.dropExtensions $ System.FilePath.takeFileName entrypoint)

      case result of
        Left err -> liftIO $ putStr err
        Right builtModules -> do
          currentDirectory <- liftIO System.Directory.getCurrentDirectory

          mapM_
            ( \((path, name), doc) -> do
                let dir =
                      System.FilePath.joinPath
                        [ Maybe.fromMaybe currentDirectory outputDir,
                          System.FilePath.joinPath (map T.unpack path)
                        ]

                liftIO $ System.Directory.createDirectoryIfMissing True dir

                liftIO
                  $ withFile
                    (System.FilePath.joinPath [dir, T.unpack name ++ ".ipe.js"])
                    WriteMode
                  $ \h ->
                    hPutDoc h doc
            )
            (Map.toList builtModules)

          preludeFiles <- liftIO Ipe.Github.fetchPrelude
          mapM_
            ( \(path, name, content) -> do
                let dir =
                      System.FilePath.joinPath
                        [ Maybe.fromMaybe currentDirectory outputDir,
                          System.FilePath.joinPath (map T.unpack path)
                        ]

                liftIO $ System.Directory.createDirectoryIfMissing True dir

                liftIO
                  $ withFile
                    (System.FilePath.joinPath [dir, T.unpack name])
                    WriteMode
                  $ \h -> ByteString.hPut h content
            )
            preludeFiles

          liftIO $ putStrLn "✅ Finished building project!"

buildModule ::
  FilePath ->
  Map ([Text], Text) Ipe.Grammar.Module ->
  Map.Map ([Text], Text) (Doc ()) ->
  ([Text], Text) ->
  Either String (Map.Map ([Text], Text) (Doc ()))
buildModule rootDir allTransformedModules builtModules currentModulePathAndName@(currentModulePath, currentModuleName) =
  case Map.lookup currentModulePathAndName builtModules of
    Just _ -> Right builtModules
    Nothing -> case Map.lookup currentModulePathAndName allTransformedModules of
      Nothing ->
        Left $
          "I was expecting to find a module at `"
            ++ System.FilePath.joinPath
              [ rootDir,
                System.FilePath.joinPath (map T.unpack currentModulePath),
                T.unpack currentModuleName ++ ".ipe"
              ]
            ++ "`, but it's not there!"
      Just transformedModule ->
        case Ipe.TypeChecker.run (Map.elems allTransformedModules) transformedModule of
          Left err -> Left err
          Right _ -> do
            let newMap = Map.insert currentModulePathAndName (Ipe.Emitter.Module.emit transformedModule) builtModules

            foldl
              ( \eitherResult currImport ->
                  case eitherResult of
                    Left err -> Left err
                    Right result ->
                      buildModule
                        rootDir
                        allTransformedModules
                        result
                        ( Ipe.Grammar.importedModulePath currImport,
                          Ipe.Grammar.importedModule currImport
                        )
              )
              (Right newMap)
              (Ipe.Grammar.moduleImports transformedModule)

-- possiblyParsedModule <- liftIO $ Ipe.Parser.parseFile entrypoint

-- case possiblyParsedModule of
--   Left err -> liftIO $ putStr err
--   Right parsedModule -> do
--     let transformedModule = Ipe.Transformer.Module.apply parsedModule

-- allParsedAndTransformedModules <- liftIO $ fetchAllImportedModules entrypoint transformedModule

-- case allParsedAndTransformedModules of
--   Left err -> liftIO $ putStr err
--   Right allModules ->
--     case Ipe.TypeChecker.run (Set.toList allModules) transformedModule of
--       Left err -> liftIO $ putStr err
--       Right types -> do
--         let modulePath = Ipe.Grammar.moduleDefinitionPath $ Ipe.Grammar.moduleDefinition parsedModule
--         let moduleName = Ipe.Grammar.moduleDefinitionName $ Ipe.Grammar.moduleDefinition parsedModule

--         liftIO $ print parsedModule
--         liftIO $ print types
--         currentDirectory <- liftIO System.Directory.getCurrentDirectory
--         absoluteOutputDir <- liftIO $ System.Directory.makeAbsolute $ Maybe.fromMaybe currentDirectory outputDir

--         let doc = Ipe.Emitter.Module.emit transformedModule
--         let outPath =
--               System.FilePath.joinPath
--                 [ absoluteOutputDir,
--                   System.FilePath.joinPath (map T.unpack modulePath),
--                   T.unpack moduleName ++ ".ipe.js"
--                 ]

--         liftIO $ System.Directory.createDirectoryIfMissing True $ System.FilePath.takeDirectory outPath

--         liftIO $ withFile outPath WriteMode $ \h -> do
--           hPutDoc h doc

fetchAllImportedAndTransformedModules ::
  Map ([Text], Text) Ipe.Grammar.Module ->
  FilePath ->
  [Text] ->
  Text ->
  IO (Either String (Map ([Text], Text) Ipe.Grammar.Module))
fetchAllImportedAndTransformedModules processedModules rootDir currentModulePath currentModuleName = do
  case Map.lookup (currentModulePath, currentModuleName) processedModules of
    Just _ -> return $ Right processedModules
    Nothing -> do
      let currPath =
            System.FilePath.joinPath
              [ rootDir,
                System.FilePath.joinPath (map T.unpack currentModulePath),
                T.unpack currentModuleName ++ ".ipe"
              ]

      possiblyParsedModule <- liftIO $ Ipe.Parser.parseFile currPath

      case possiblyParsedModule of
        Left err -> return $ Left err
        Right parsedModule -> do
          let transformedModule = Ipe.Transformer.Module.apply parsedModule

          Control.Monad.foldM
            ( \eitherResult currImport ->
                case eitherResult of
                  Left err -> return $ Left err
                  Right currResult -> do
                    childResult <-
                      fetchAllImportedAndTransformedModules
                        currResult
                        rootDir
                        (Ipe.Grammar.importedModulePath currImport)
                        (Ipe.Grammar.importedModule currImport)

                    case childResult of
                      Left err -> return $ Left err
                      Right childValue ->
                        return $ Right $ Map.union currResult childValue
            )
            (Right $ Map.insert (currentModulePath, currentModuleName) transformedModule processedModules)
            (Ipe.Grammar.moduleImports parsedModule)

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app
