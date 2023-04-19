module Ipe.TypeChecker (run) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Ipe.Grammar
import qualified Ipe.TypeChecker.Module
import qualified Ipe.TypeChecker.Utils

run :: [Ipe.Grammar.Module] -> Ipe.Grammar.Module -> Either String (Map.Map Text Ipe.TypeChecker.Utils.Type)
run otherModules currModule =
  case Ipe.TypeChecker.Module.run otherModules currModule of
    Left err -> Left $ show err
    Right ok -> Right ok
