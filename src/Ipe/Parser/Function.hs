{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.Function (parser) where

import qualified Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import qualified Ipe.Parser.Expression
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

parser :: Parser Ipe.Grammar.IpeFunction
parser = do
  Control.Monad.void $ Ipe.Parser.symbol "\\"

  arguments <- Parsec.Common.some (Ipe.Parser.lexeme fnArg)

  Control.Monad.void $ Ipe.Parser.symbol "->"

  attributions <- Parsec.Common.many $ Parsec.Common.try attribution

  returnExpr <- Ipe.Parser.Expression.parser

  return $
    Ipe.Grammar.IpeFunction
      { Ipe.Grammar.arguments = arguments,
        Ipe.Grammar.functionBody =
          Ipe.Grammar.IpeFunctionBody
            { Ipe.Grammar.attributions = attributions,
              Ipe.Grammar.functionReturn = returnExpr
            }
      }

fnArg :: Parser Text
fnArg = do
  firstChar <- Parsec.Char.lowerChar
  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
      )

  return $ T.pack $ firstChar : rest

attributionName :: Parser Text
attributionName = do
  firstChar <- Parsec.Char.lowerChar
  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
      )

  return $ T.pack $ firstChar : rest

attribution :: Parser (Text, Ipe.Grammar.Expression)
attribution = do
  name <- Ipe.Parser.lexeme attributionName

  Control.Monad.void $ Ipe.Parser.symbol "="

  expression <- Ipe.Parser.Expression.parser

  Control.Monad.void $ Ipe.Parser.symbol ";"

  return (name, expression)
