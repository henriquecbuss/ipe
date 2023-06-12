{-# LANGUAGE ApplicativeDo #-}

module Ipe.Cli (Options (..), optionsP) where

import Data.Text (Text)
import qualified Options.Applicative as Opt

data Options = Build
  { entrypoint :: FilePath,
    outputDir :: Maybe FilePath,
    preludeBranch :: Maybe Text
  }

optionsP :: Opt.Parser Options
optionsP = Opt.subparser buildCommand

buildCommand :: Opt.Mod Opt.CommandFields Options
buildCommand =
  Opt.command
    "build"
    ( Opt.info
        (Opt.helper <*> buildP)
        ( Opt.fullDesc
            <> Opt.progDesc "Build an Ipe project"
        )
    )

buildP :: Opt.Parser Options
buildP = do
  rootFile <-
    fileArgument
      ( Opt.metavar "ROOT_FILE"
          <> Opt.help "Path to the root file. Ex: `./src/root.ipe`"
      )

  output <-
    Opt.optional $
      fileOption $
        Opt.long "output-dir"
          <> Opt.short 'o'
          <> Opt.metavar "OUTPUT_DIR"
          <> Opt.help "Path to the output directory. Ex: `./dist`"

  preludeBranchOpt <-
    Opt.optional $
      Opt.strOption $
        Opt.long "prelude-branch"
          <> Opt.metavar "PRELUDE_BRANCH"
          <> Opt.help "(optional) Branch to fetch the prelude from"

  return Build {entrypoint = rootFile, outputDir = output, preludeBranch = preludeBranchOpt}

fileArgument :: Opt.Mod Opt.ArgumentFields FilePath -> Opt.Parser FilePath
fileArgument = Opt.strArgument

fileOption :: Opt.Mod Opt.OptionFields FilePath -> Opt.Parser FilePath
fileOption = Opt.strOption
