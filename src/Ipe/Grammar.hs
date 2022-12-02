module Ipe.Grammar
  ( ModuleDefinition (..),
  )
where

import Data.Text (Text)

data ModuleDefinition = ModuleDefinition
  { moduleDefinitionName :: Text,
    exportedDefinitions :: [Text],
    moduleDocComment :: Maybe Text
  }
  deriving (Eq, Show)
