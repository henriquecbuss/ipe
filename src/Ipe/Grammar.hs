module Ipe.Grammar
  ( ModuleDefinition (..),
    Expression (..),
  )
where

import Data.Text (Text)

data ModuleDefinition = ModuleDefinition
  { moduleDefinitionName :: Text,
    exportedDefinitions :: [Text],
    moduleDocComment :: Maybe Text
  }
  deriving (Eq, Show)

data Expression = Expression
