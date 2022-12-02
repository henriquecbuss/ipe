module Ipe.Grammar
  ( ModuleDefinition (..),
    ImportList,
    ImportExpression (..),
  )
where

import Data.Text (Text)

data ModuleDefinition = ModuleDefinition
  { moduleDefinitionName :: Text,
    exportedDefinitions :: [Text],
    moduleDocComment :: Maybe Text
  }
  deriving (Eq, Show)

type ImportList = [ImportExpression]

data ImportExpression = ImportExpression
  { importedModule :: Text,
    importedModuleAlias :: Maybe Text
  }
  deriving (Eq, Show)
