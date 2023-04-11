{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Ipe.Cli (Options (..), optionsP) where

import Data.Text (Text)
import qualified Options.Applicative as Opt

data Options = Options
  { optionsFile :: FilePath,
    optionsSearch :: Text
  }

optionsP :: Opt.Parser Options
optionsP = do
  optionsFile <-
    Opt.strOption $
      mconcat
        [ Opt.long "file",
          Opt.short 'f',
          Opt.metavar "FILE_PATH",
          Opt.help "Path to the file"
        ]

  optionsSearch <-
    Opt.strOption $
      mconcat
        [ Opt.long "search",
          Opt.short 's',
          Opt.metavar "STRING",
          Opt.help "Substring to find and highlight"
        ]

  pure Options {..}
