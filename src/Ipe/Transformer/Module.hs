{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ipe.Transformer.Module (apply) where

import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Ipe.Grammar (CustomTypeConstructor (..), Module (..), ModuleDefinition (..), TypeDefinition (..), TypeUnion (..))

apply :: Module -> Module
apply = exportTypeUnionConstructors

exportTypeUnionConstructors :: Module -> Module
exportTypeUnionConstructors m@(Module {typeDefinitions, moduleDefinition}) =
  m
    { moduleDefinition =
        moduleDefinition
          { exportedDefinitions =
              exportedDefinitions moduleDefinition
                ++ typeUnionConstructorFunctions
          }
    }
  where
    typeUnionConstructorFunctions :: [Text]
    typeUnionConstructorFunctions =
      concat $
        Maybe.mapMaybe
          ( \case
              TypeAliasDefinition _ -> Nothing
              TypeOpaqueDefinition _ -> Nothing
              TypeUnionDefinition (TypeUnion {typeUnionDefinitionName, typeUnionDefinitionConstructors}) ->
                if typeUnionDefinitionName `elem` exportedDefinitions moduleDefinition
                  then Just $ map customTypeConstructorName typeUnionDefinitionConstructors
                  else Nothing
          )
          typeDefinitions
