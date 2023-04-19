module Ipe.Settings (appSettings) where

import Ipe.Cli (Options, optionsP)
import qualified Iris
import Paths_ipe as Autogen
import qualified System.Console.Pretty as Pretty

appSettings :: Iris.CliEnvSettings Options ()
appSettings =
  Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = Pretty.style Pretty.Bold $ Pretty.color Pretty.Green "Ipe",
      Iris.cliEnvSettingsProgDesc = "A " <> Pretty.color Pretty.Cyan "functional" <> " programming language for backend apps",
      Iris.cliEnvSettingsAppName = Just "Ipe",
      Iris.cliEnvSettingsVersionSettings =
        Just
          (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = ("Ipe v" <>)
            },
      Iris.cliEnvSettingsCmdParser = optionsP
    }
