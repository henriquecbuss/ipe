{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.Expression (parser) where

import qualified Control.Monad
import qualified Control.Monad.Combinators.Expr as Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char
import qualified Text.Megaparsec.Char.Lexer as Parsec.Lexer

parser :: Parser Ipe.Grammar.Expression
parser =
  Combinators.Expr.makeExprParser (term True) operatorTable

literalNumber :: Parser Float
literalNumber =
  Parsec.Common.choice
    [ (* (-1)) <$> Parsec.Common.try (Parsec.Char.char '-' *> Parsec.Lexer.float),
      (* (-1)) <$> (Parsec.Char.char '-' *> Parsec.Lexer.decimal),
      Parsec.Common.try Parsec.Lexer.float,
      Parsec.Lexer.decimal
    ]

literalString :: Parser Text
literalString =
  T.pack
    <$> ( Parsec.Char.char '\''
            *> Parsec.Common.manyTill
              Parsec.Lexer.charLiteral
              (Parsec.Char.char '\'')
        )

functionCallOrValue :: Bool -> Parser Ipe.Grammar.Expression
functionCallOrValue acceptArgs = do
  moduleNames <- Parsec.Common.many $ Parsec.Common.try (Ipe.Parser.uppercaseIdentifier <* Parsec.Char.char '.')

  (name, recordAccessors) <-
    Ipe.Parser.lexeme
      ( do
          name <- Ipe.Parser.lowercaseIdentifier <|> Ipe.Parser.uppercaseIdentifier

          recordAccessors <- Parsec.Common.many (Parsec.Char.char '.' *> Ipe.Parser.lowercaseIdentifier)
          return (name, recordAccessors)
      )

  args <-
    if acceptArgs
      then Parsec.Common.many . Parsec.Common.try . Ipe.Parser.lexeme $ term False
      else return []

  return $
    Ipe.Grammar.IpeFunctionCallOrValue
      ( Ipe.Grammar.FunctionCallOrValue
          { Ipe.Grammar.functionCallOrValuePath = moduleNames,
            Ipe.Grammar.functionCallOrValueName = name,
            Ipe.Grammar.functionCallOrValueRecordAccessors = recordAccessors,
            Ipe.Grammar.functionCallOrValueArguments = args
          }
      )

term :: Bool -> Parser Ipe.Grammar.Expression
term acceptArgs =
  Parsec.Common.choice
    [ Parsec.Common.between (Ipe.Parser.symbol "(") (Ipe.Parser.symbol ")") parser,
      Ipe.Parser.lexeme $ Ipe.Grammar.IpeNumber <$> literalNumber,
      Ipe.Parser.lexeme $ Ipe.Grammar.IpeString <$> literalString,
      Ipe.Parser.lexeme function,
      Ipe.Parser.lexeme matchExpression,
      functionCallOrValue acceptArgs
    ]
    <* Parsec.Common.notFollowedBy
      ( Parsec.Common.choice $ fmap Ipe.Parser.symbol forbiddenSymbols
      )

operatorTable :: [[Combinators.Expr.Operator Parser Ipe.Grammar.Expression]]
operatorTable =
  [ [binary "^" Ipe.Grammar.Exponentiation],
    [ binary "*" Ipe.Grammar.Multiply,
      binary "/" Ipe.Grammar.Divide
    ],
    [ binary "+" Ipe.Grammar.Add,
      binary "-" Ipe.Grammar.Subtract
    ],
    [ binary "|>" Ipe.Grammar.PipeRight,
      binary "<|" Ipe.Grammar.PipeLeft
    ]
  ]

binary ::
  Text ->
  Ipe.Grammar.IpeBinaryOperator ->
  Combinators.Expr.Operator Parser Ipe.Grammar.Expression
binary name constructor =
  Combinators.Expr.InfixL (Ipe.Grammar.IpeBinaryOperation constructor <$ Ipe.Parser.symbol name)

function :: Parser Ipe.Grammar.Expression
function = do
  Control.Monad.void $ Ipe.Parser.symbol "\\"

  arguments <- Parsec.Common.some (Ipe.Parser.lexeme Ipe.Parser.lowercaseIdentifier)

  Control.Monad.void $ Ipe.Parser.symbol "->"

  attributions <- Parsec.Common.many $ Parsec.Common.try functionAttribution

  returnExpr <- Ipe.Parser.Expression.parser

  return $
    Ipe.Grammar.IpeFunction
      arguments
      ( Ipe.Grammar.IpeFunctionBody
          { Ipe.Grammar.attributions = attributions,
            Ipe.Grammar.functionReturn = returnExpr
          }
      )

functionAttributionName :: Parser Text
functionAttributionName = do
  firstChar <- Parsec.Char.lowerChar
  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
      )

  return $ T.pack $ firstChar : rest

functionAttribution :: Parser (Text, Ipe.Grammar.Expression)
functionAttribution = do
  name <- Ipe.Parser.lexeme functionAttributionName

  Control.Monad.void $ Ipe.Parser.symbol "="

  expression <- Ipe.Parser.Expression.parser

  Control.Monad.void $ Ipe.Parser.symbol ";"

  return (name, expression)

matchExpression :: Parser Ipe.Grammar.Expression
matchExpression = do
  Control.Monad.void $ Ipe.Parser.symbol "match"

  expression <- Ipe.Parser.Expression.parser

  Control.Monad.void $ Ipe.Parser.symbol "with"

  matchCases <- Parsec.Common.some matchCase

  return $ Ipe.Grammar.IpeMatch expression matchCases

matchCase :: Parser (Ipe.Grammar.IpeMatchPattern, Ipe.Grammar.Expression)
matchCase = do
  Control.Monad.void $ Ipe.Parser.symbol "|"

  pattern_ <- Ipe.Parser.lexeme $ matchPattern True

  Control.Monad.void $ Ipe.Parser.symbol "->"

  expression <- Ipe.Parser.Expression.parser

  return (pattern_, expression)

matchPattern :: Bool -> Parser Ipe.Grammar.IpeMatchPattern
matchPattern acceptArgs =
  Parsec.Common.choice
    [ Parsec.Common.between (Ipe.Parser.symbol "(") (Ipe.Parser.symbol ")") (matchPattern True),
      customTypePattern acceptArgs,
      Ipe.Grammar.IpeLiteralNumberPattern <$> literalNumber,
      Ipe.Grammar.IpeLiteralStringPattern <$> literalString,
      Ipe.Grammar.IpeVariablePattern <$> Ipe.Parser.lowercaseIdentifier,
      Ipe.Grammar.IpeWildCardPattern <$ Parsec.Char.char '_'
    ]

customTypePattern :: Bool -> Parser Ipe.Grammar.IpeMatchPattern
customTypePattern acceptArgs = do
  customTypePath <- Parsec.Common.many $ Parsec.Common.try (Ipe.Parser.uppercaseIdentifier <* Parsec.Char.char '.')
  customTypeName <- Ipe.Parser.lexeme Ipe.Parser.uppercaseIdentifier

  args <-
    if acceptArgs
      then Parsec.Common.many . Parsec.Common.try . Ipe.Parser.lexeme $ matchPattern False
      else return []

  return $
    Ipe.Grammar.IpeCustomTypePattern
      customTypePath
      customTypeName
      args

forbiddenSymbols :: [Text]
forbiddenSymbols = [":"]
