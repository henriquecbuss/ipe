{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.Import (singleParser, listParser) where

import qualified Control.Monad
import qualified Ipe.Grammar
import Ipe.Parser.Utils (Parser)
import qualified Ipe.Parser.Utils as Ipe.Parser
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

listParser :: Parser Ipe.Grammar.ImportList
listParser =
  Parsec.Common.many
    ( do
        importExpression <- singleParser
        Control.Monad.void Parsec.Char.eol <|> Parsec.Common.eof

        return importExpression
    )

singleParser :: Parser Ipe.Grammar.ImportExpression
singleParser = do
  Control.Monad.void (Ipe.Parser.symbol "import")

  (path, name) <- Ipe.Parser.hlexeme Ipe.Parser.moduleName

  alias <-
    Parsec.Common.optional
      ( Ipe.Parser.symbol "as"
          *> (Ipe.Parser.hlexeme Ipe.Parser.moduleName <?> "the alias for the imported module. It must be a valid module name, starting with an upper case letter, and containing only letters, numbers, `.` or `_`")
      )

  return $
    Ipe.Grammar.ImportExpression
      { Ipe.Grammar.importedModulePath = path,
        Ipe.Grammar.importedModule = name,
        Ipe.Grammar.importedModuleAlias = alias
      }
