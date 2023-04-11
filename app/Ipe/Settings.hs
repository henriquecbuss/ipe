module Ipe.Settings (appSettings) where

import Ipe.Cli (Options, optionsP)
import qualified Iris
import Paths_ipe as Autogen

appSettings :: Iris.CliEnvSettings Options ()
appSettings =
  Iris.defaultCliEnvSettings
    { -- short description
      Iris.cliEnvSettingsHeaderDesc = "Iris usage example",
      -- longer description
      Iris.cliEnvSettingsProgDesc = "A simple grep utility - tutorial example",
      -- a function to display the tool version
      Iris.cliEnvSettingsVersionSettings =
        Just
          (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = ("Simple grep utility v" <>)
            },
      -- our 'Options' CLI parser
      Iris.cliEnvSettingsCmdParser = optionsP
    }
