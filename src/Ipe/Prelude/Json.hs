{-# LANGUAGE OverloadedStrings #-}

module Ipe.Prelude.Json (moduleTypes, typeDef) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.TypeChecker.Utils (Type (..))

moduleTypes :: Map.Map Text Type
moduleTypes =
  Map.singleton "Value" typeDef

typeDef :: Type
typeDef = TCustom "Value" [] []
