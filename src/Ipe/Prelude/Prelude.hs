{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Prelude (registerAllModules, allModuleNames) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Ipe.Prelude.Console
import qualified Ipe.Prelude.Dict
import qualified Ipe.Prelude.Http
import qualified Ipe.Prelude.Json
import qualified Ipe.Prelude.Json.Decode
import qualified Ipe.Prelude.Json.Encode
import qualified Ipe.Prelude.List
import qualified Ipe.Prelude.Number
import qualified Ipe.Prelude.Promise
import qualified Ipe.TypeChecker.Utils

registerAllModules :: Map.Map Text (Map.Map Text Ipe.TypeChecker.Utils.Type)
registerAllModules =
  Map.fromList $
    map
      ( \(moduleName, moduleTypes) ->
          ( moduleName,
            Map.map (Ipe.TypeChecker.Utils.prefix [] moduleName) $
              Map.mapKeys (\name -> moduleName <> "." <> name) moduleTypes
          )
      )
      [ ("Dict", Ipe.Prelude.Dict.moduleTypes),
        ("Json", Ipe.Prelude.Json.moduleTypes),
        ("Json.Encode", Ipe.Prelude.Json.Encode.moduleTypes),
        ("Json.Decode", Ipe.Prelude.Json.Decode.moduleTypes),
        ("Console", Ipe.Prelude.Console.moduleTypes),
        ("Promise", Ipe.Prelude.Promise.moduleTypes),
        ("Http", Ipe.Prelude.Http.moduleTypes),
        ("List", Ipe.Prelude.List.moduleTypes),
        ("Number", Ipe.Prelude.Number.moduleTypes)
      ]

allModuleNames :: [Text]
allModuleNames = Map.keys registerAllModules
