{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.TypeDefinition (parser, ipeType) where

import qualified Control.Monad
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Ipe.Grammar
import Ipe.Parser (Parser)
import qualified Ipe.Parser
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

parser :: Parser Ipe.Grammar.TypeDefinition
parser = do
  docComment <- Parsec.Common.optional Ipe.Parser.docComment

  Control.Monad.void (Ipe.Parser.symbol "type")

  Parsec.Common.choice
    [ typeAlias docComment,
      typeUnion docComment,
      typeOpaque docComment
    ]

typeAlias :: Maybe Text -> Parser Ipe.Grammar.TypeDefinition
typeAlias docComment = do
  Control.Monad.void (Ipe.Parser.symbol "alias")

  typeAliasName <- Ipe.Parser.lexeme Ipe.Parser.uppercaseIdentifier <?> "a type definition name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`"

  typeAliasParameters <- Parsec.Common.many (Ipe.Parser.lexeme Ipe.Parser.lowercaseIdentifier <?> "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`")

  Control.Monad.void (Ipe.Parser.symbol "=") <?> "an `=`, followed by the actual type definition"

  typeAliasType <- ipeType True

  return $
    Ipe.Grammar.TypeAliasDefinition
      ( Ipe.Grammar.TypeAlias
          { Ipe.Grammar.typeAliasDefinitionName = typeAliasName,
            Ipe.Grammar.typeAliasDefinitionParameters = typeAliasParameters,
            Ipe.Grammar.typeAliasDefinitionDocComment = docComment,
            Ipe.Grammar.typeAliasType = typeAliasType
          }
      )

typeUnion :: Maybe Text -> Parser Ipe.Grammar.TypeDefinition
typeUnion docComment =
  customTypeWithConstructors
    "union"
    docComment
    ( \name params doc type_ ->
        Ipe.Grammar.TypeUnionDefinition $
          Ipe.Grammar.TypeUnion name params doc type_
    )

typeOpaque :: Maybe Text -> Parser Ipe.Grammar.TypeDefinition
typeOpaque docComment =
  customTypeWithConstructors
    "opaque"
    docComment
    ( \name params doc type_ ->
        Ipe.Grammar.TypeOpaqueDefinition $
          Ipe.Grammar.TypeOpaque name params doc type_
    )

customTypeWithConstructors ::
  Text ->
  Maybe Text ->
  ( Text ->
    [Text] ->
    Maybe Text ->
    [Ipe.Grammar.CustomTypeConstructor] ->
    Ipe.Grammar.TypeDefinition
  ) ->
  Parser Ipe.Grammar.TypeDefinition
customTypeWithConstructors typeName docComment buildType = do
  Control.Monad.void (Ipe.Parser.symbol typeName)

  name <- Ipe.Parser.lexeme Ipe.Parser.uppercaseIdentifier <?> "a type definition name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`"

  parameters <- Parsec.Common.many (Ipe.Parser.lexeme Ipe.Parser.lowercaseIdentifier <?> "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`")

  Control.Monad.void (Ipe.Parser.symbol "=") <?> "the actual type definition"

  constructors <- Parsec.Common.some $ Parsec.Common.try customTypeConstructor

  return $ buildType name parameters docComment constructors

ipeType :: Bool -> Parser Ipe.Grammar.IpeType
ipeType acceptArgs =
  Parsec.Common.choice
    [ Ipe.Parser.symbol "(" *> Ipe.Parser.lexeme (ipeType True) <* (Ipe.Parser.symbol ")" <?> "a closing parenthesis (`)`)"),
      Ipe.Parser.lexeme . Parsec.Common.try $ Ipe.Grammar.ParameterType <$> Ipe.Parser.lowercaseIdentifier,
      Ipe.Parser.lexeme $ concreteType acceptArgs,
      Ipe.Parser.lexeme recordType
    ]
    <?> "a type, which can start with an uppercase or lowercase letter, or a record, with fields inside curly brackets (`{` and `}`)"

concreteType :: Bool -> Parser Ipe.Grammar.IpeType
concreteType acceptArgs = do
  importedFrom <- Parsec.Common.many $ Parsec.Common.try (Ipe.Parser.uppercaseIdentifier <* Parsec.Char.char '.')

  name <- Ipe.Parser.lexeme Ipe.Parser.uppercaseIdentifier

  args <-
    if acceptArgs
      then Parsec.Common.many (ipeType False)
      else return []

  return $ Ipe.Grammar.ConcreteType importedFrom name args

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
  itemName <- Ipe.Parser.lexeme Ipe.Parser.lowercaseIdentifier

  Control.Monad.void (Ipe.Parser.symbol ":")

  itemType <- ipeType True

  return (itemName, itemType)

customTypeConstructor :: Parser Ipe.Grammar.CustomTypeConstructor
customTypeConstructor = do
  docComment <- Parsec.Common.optional Ipe.Parser.docComment <?> "a doc comment, starting with `/|*` and ending with `*/`"

  Control.Monad.void (Ipe.Parser.symbol "|" <?> "a `|`, followed by a constructor name")

  name <- Ipe.Parser.lexeme Ipe.Parser.uppercaseIdentifier <?> "a type constructor name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`"

  customTypeArgs <- Parsec.Common.many $ ipeType False

  return $
    Ipe.Grammar.CustomTypeConstructor
      { Ipe.Grammar.customTypeConstructorName = name,
        Ipe.Grammar.customTypeConstructorDocComment = docComment,
        Ipe.Grammar.customTypeConstructorArgs = customTypeArgs
      }
