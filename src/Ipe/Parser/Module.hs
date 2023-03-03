module Ipe.Parser.Module (parser) where

import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import qualified Ipe.Parser.Import
import qualified Ipe.Parser.ModuleDefinition
import qualified Ipe.Parser.TopLevelDefinition
import qualified Ipe.Parser.TypeDefinition
import qualified Text.Megaparsec as Parsec.Common

parser :: Parser Ipe.Grammar.Module
parser = do
  moduleDefinition <- Ipe.Parser.ModuleDefinition.parser

  moduleImports <- Ipe.Parser.Import.listParser

  (typeDefinitions, topLevelDefinitions) <- splitModuleBody <$> moduleBody

  return $
    Ipe.Grammar.Module
      { Ipe.Grammar.moduleDefinition = moduleDefinition,
        Ipe.Grammar.moduleImports = moduleImports,
        Ipe.Grammar.typeDefinitions = typeDefinitions,
        Ipe.Grammar.topLevelDefinitions = topLevelDefinitions
      }

moduleBody :: Parser [TopLevelDefinitionKind]
moduleBody =
  Parsec.Common.many
    ( Ipe.Parser.lexeme $
        Parsec.Common.choice
          [ TypeDefinition <$> Ipe.Parser.TypeDefinition.parser,
            TopLevelDefinition <$> Ipe.Parser.TopLevelDefinition.parser
          ]
    )

splitModuleBody :: [TopLevelDefinitionKind] -> ([Ipe.Grammar.TypeDefinition], [Ipe.Grammar.TopLevelDefinition])
splitModuleBody = foldr split ([], [])
  where
    split (TypeDefinition typeDefinition) (typeDefinitions, topLevelDefinitions) =
      (typeDefinition : typeDefinitions, topLevelDefinitions)
    split (TopLevelDefinition topLevelDefinition) (typeDefinitions, topLevelDefinitions) =
      (typeDefinitions, topLevelDefinition : topLevelDefinitions)

data TopLevelDefinitionKind
  = TypeDefinition Ipe.Grammar.TypeDefinition
  | TopLevelDefinition Ipe.Grammar.TopLevelDefinition
