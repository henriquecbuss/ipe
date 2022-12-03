{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.TypeDefinition (parser) where

import qualified Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

parser :: Parser Ipe.Grammar.TypeDefinition
parser =
  Parsec.Common.choice
    [ typeAlias,
      typeUnion,
      typeOpaque
    ]

typeAlias :: Parser Ipe.Grammar.TypeDefinition
typeAlias = do
  docComment <- Parsec.Common.optional Ipe.Parser.docComment

  Control.Monad.void (Ipe.Parser.symbol "type")
  Control.Monad.void (Ipe.Parser.symbol "alias")

  typeAliasName <- Ipe.Parser.lexeme typeDefinitionName

  typeAliasParameters <- Parsec.Common.many (Ipe.Parser.lexeme typeDefinitionParameter)

  Control.Monad.void (Ipe.Parser.symbol "=") <?> "the actual type definition"

  typeAliasType <- ipeType True

  return $
    Ipe.Grammar.TypeAliasDefinition
      { Ipe.Grammar.typeAliasDefinitionName = typeAliasName,
        Ipe.Grammar.typeAliasDefinitionParameters = typeAliasParameters,
        Ipe.Grammar.typeAliasDefinitionDocComment = docComment,
        Ipe.Grammar.typeAliasType = typeAliasType
      }

typeUnion :: Parser Ipe.Grammar.TypeDefinition
typeUnion = Parsec.Common.hidden $ Parsec.Common.failure Nothing Set.empty

typeOpaque :: Parser Ipe.Grammar.TypeDefinition
typeOpaque = Parsec.Common.hidden $ Parsec.Common.failure Nothing Set.empty

typeDefinitionName :: Parser Text
typeDefinitionName = do
  firstChar <- Parsec.Char.upperChar <?> "a type definition name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`"

  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
          <?> "the rest of the type definition name, which can be any combination of letters, numbers or `_`"
      )

  return $ T.pack $ firstChar : rest

typeDefinitionParameter :: Parser Text
typeDefinitionParameter = do
  firstChar <-
    Parsec.Char.lowerChar
      <?> "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"

  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
      )

  return $ T.pack $ firstChar : rest

ipeType :: Bool -> Parser Ipe.Grammar.IpeType
ipeType acceptArgs =
  Parsec.Common.choice
    [ Ipe.Parser.symbol "(" *> ipeType True <* (Ipe.Parser.symbol ")" <?> "a closing parenthesis (`)`)"),
      parameterType,
      concreteType acceptArgs,
      recordType
    ]
    <?> "a type, which can start with an uppercase or lowercase letter, or a record, with fields inside curly brackets (`{` and `}`)"

parameterType :: Parser Ipe.Grammar.IpeType
parameterType = do
  firstChar <- Parsec.Char.lowerChar

  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
      )

  return $ Ipe.Grammar.ParameterType $ T.pack $ firstChar : rest

concreteType :: Bool -> Parser Ipe.Grammar.IpeType
concreteType acceptArgs = do
  name <-
    Ipe.Parser.lexeme
      ( do
          firstChar <- Parsec.Char.upperChar

          rest <-
            Parsec.Common.many
              ( Parsec.Common.choice
                  [ Parsec.Char.alphaNumChar,
                    Parsec.Char.char '_',
                    Parsec.Char.char '.'
                  ]
              )

          return $ T.pack $ firstChar : rest
      )

  args <-
    if acceptArgs
      then Parsec.Common.many (ipeType False)
      else return []

  return $ Ipe.Grammar.ConcreteType name args

recordType :: Parser Ipe.Grammar.IpeType
recordType = do
  Control.Monad.void (Ipe.Parser.symbol "{")

  firstItem <- Parsec.Common.optional recordItem
  otherItems <- Parsec.Common.many (Ipe.Parser.symbol "," *> recordItem)

  Control.Monad.void (Ipe.Parser.symbol "}")

  items <- case firstItem of
    Nothing -> return Map.empty
    Just item -> return $ Map.fromList $ item : otherItems

  return $ Ipe.Grammar.RecordType items

recordItem :: Parser (Text, Ipe.Grammar.IpeType)
recordItem = do
  itemName <-
    Ipe.Parser.lexeme
      ( do
          firstChar <- Parsec.Char.lowerChar

          rest <-
            Parsec.Common.many
              ( Parsec.Common.choice
                  [ Parsec.Char.alphaNumChar,
                    Parsec.Char.char '_'
                  ]
              )

          return $ T.pack $ firstChar : rest
      )

  Control.Monad.void (Ipe.Parser.symbol ":")

  itemType <- ipeType True

  return (itemName, itemType)
