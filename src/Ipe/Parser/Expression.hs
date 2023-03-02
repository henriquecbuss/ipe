{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.Expression (parser) where

import qualified Control.Monad
import qualified Control.Monad.Combinators.Expr as Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char
import qualified Text.Megaparsec.Char.Lexer as Parsec.Lexer

parser :: Parser Ipe.Grammar.Expression
parser =
  Combinators.Expr.makeExprParser (term True) operatorTable

number :: Parser Ipe.Grammar.Expression
number =
  Ipe.Grammar.IpeNumber
    <$> Parsec.Common.choice
      [ (* (-1)) <$> Parsec.Common.try (Parsec.Char.char '-' *> Parsec.Lexer.float),
        (* (-1)) <$> (Parsec.Char.char '-' *> Parsec.Lexer.decimal),
        Parsec.Common.try Parsec.Lexer.float,
        Parsec.Lexer.decimal
      ]

string :: Parser Ipe.Grammar.Expression
string =
  Ipe.Grammar.IpeString . T.pack
    <$> ( Parsec.Char.char '\''
            *> Parsec.Common.manyTill
              Parsec.Lexer.charLiteral
              (Parsec.Char.char '\'')
        )

importedValue :: Parser Text
importedValue = do
  moduleNames <-
    Parsec.Common.many
      ( do
          firstModuleChar <- Parsec.Char.upperChar
          restOfModule <-
            Parsec.Common.many
              ( Parsec.Common.choice
                  [ Parsec.Char.alphaNumChar,
                    Parsec.Char.char '_'
                  ]
              )

          Control.Monad.void (Parsec.Char.char '.')
          return $ T.pack (firstModuleChar : restOfModule)
      )

  functionName <- do
    firstFunctionChar <- Parsec.Char.lowerChar
    restOfFunction <-
      Parsec.Common.many
        ( Parsec.Common.choice
            [ Parsec.Char.alphaNumChar,
              Parsec.Char.char '_'
            ]
        )

    return $ T.pack (firstFunctionChar : restOfFunction)

  return $ T.intercalate "." (moduleNames ++ [functionName])

functionCallOrValue :: Bool -> Parser Ipe.Grammar.Expression
functionCallOrValue acceptArgs = do
  name <- Ipe.Parser.lexeme importedValue

  args <-
    if acceptArgs
      then Parsec.Common.many $ Ipe.Parser.lexeme $ term False
      else return []

  return $ Ipe.Grammar.IpeFunctionCallOrValue name args

term :: Bool -> Parser Ipe.Grammar.Expression
term acceptArgs =
  Parsec.Common.choice
    [ Parsec.Common.between (Ipe.Parser.symbol "(") (Ipe.Parser.symbol ")") parser,
      Ipe.Parser.lexeme number,
      Ipe.Parser.lexeme string,
      Ipe.Parser.lexeme function,
      functionCallOrValue acceptArgs
    ]

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

  arguments <- Parsec.Common.some (Ipe.Parser.lexeme functionArg)

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

functionArg :: Parser Text
functionArg = do
  firstChar <- Parsec.Char.lowerChar
  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
      )

  return $ T.pack $ firstChar : rest

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
