{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Prelude (registerAllModules, allModuleNames) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Ipe.Prelude.Dict
import qualified Ipe.TypeChecker.Utils

registerAllModules :: Map.Map Text Ipe.TypeChecker.Utils.Type
registerAllModules =
  Map.unions $
    map
      ( \(moduleName, moduleTypes) ->
          Map.mapKeys (\name -> moduleName <> "." <> name) moduleTypes
      )
      [ ("Dict", Ipe.Prelude.Dict.moduleTypes)
      ]

allModuleNames :: [Text]
allModuleNames = ["Dict"]
