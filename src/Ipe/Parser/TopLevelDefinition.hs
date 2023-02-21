{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.TopLevelDefinition (parser) where

import qualified Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import qualified Ipe.Parser.Expression
import qualified Ipe.Parser.Function
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

parser :: Parser Ipe.Grammar.TopLevelDefinition
parser = do
  docComment <- Parsec.Common.optional Ipe.Parser.docComment

  name <- Ipe.Parser.lexeme tldName

  Control.Monad.void (Ipe.Parser.symbol "=")

  value <-
    Parsec.Common.choice
      [ Ipe.Grammar.TopLevelExpression <$> Ipe.Parser.Expression.parser,
        Ipe.Grammar.TopLevelFunction <$> Ipe.Parser.Function.parser
      ]

  return $
    Ipe.Grammar.TopLevelDefinition
      { Ipe.Grammar.topLevelDefinitionName = name,
        Ipe.Grammar.topLevelDefinitionDocComment = docComment,
        Ipe.Grammar.topLevelDefinitionValue = value
      }

tldName :: Parser Text
tldName = do
  firstChar <- Parsec.Char.lowerChar
  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
      )

  return $ T.pack $ firstChar : rest
