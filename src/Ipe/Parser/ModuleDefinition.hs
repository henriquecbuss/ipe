{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.ModuleDefinition (parser) where

import qualified Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

parser :: Parser Ipe.Grammar.ModuleDefinition
parser = do
  Control.Monad.void (Ipe.Parser.symbol "module")

  name <- Ipe.Parser.lexeme Ipe.Parser.moduleName <?> "a module name. Module names must start with an upper case letter, and contain only letters, numbers, `.` or `_`"

  Control.Monad.void (Ipe.Parser.symbol "exports") <?> "the `exports` keyword, followed by a list of exported definitions"

  Control.Monad.void (Ipe.Parser.symbol "[") <?> "the list of exported definitions. It must be a list of comma-separated items, surrounded by square brackets (`[` and `]`). For example: `module SomeModule exports [ someFunction, someOtherFunction ]"

  firstExportedDefinition <- Ipe.Parser.lexeme exportedDefinition

  otherExportedDefinitions <-
    Parsec.Common.many
      ( (Ipe.Parser.symbol "," <?> "more items in the list")
          *> Ipe.Parser.lexeme exportedDefinition
      )

  Control.Monad.void (Ipe.Parser.symbolWithNoBlockComments "]") <?> "the closing square bracket (`]`) to signify the end of the list"

  moduleDocComment <- Parsec.Common.optional Ipe.Parser.docComment

  return $
    Ipe.Grammar.ModuleDefinition
      { Ipe.Grammar.moduleDefinitionName = name,
        Ipe.Grammar.exportedDefinitions = firstExportedDefinition : otherExportedDefinitions,
        Ipe.Grammar.moduleDocComment = moduleDocComment
      }

exportedDefinition :: Parser Text
exportedDefinition =
  ( do
      firstChar <- Parsec.Char.upperChar <|> Parsec.Char.lowerChar

      rest <-
        Parsec.Common.many
          ( ( Parsec.Char.alphaNumChar
                <|> Parsec.Char.char '_'
            )
              <?> "the rest of the definition's name"
          )

      return $ T.pack $ firstChar : rest
  )
    <?> "a top level definition name. Top level definition names must start with a letter (lowercase for functions or uppercase for types), and contain only letters, numbers, `.` or `_`"
