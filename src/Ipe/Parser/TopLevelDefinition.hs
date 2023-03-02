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
import qualified Ipe.Parser.TypeDefinition
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

parser :: Parser Ipe.Grammar.TopLevelDefinition
parser = do
  docComment <- Parsec.Common.optional Ipe.Parser.docComment

  optionalAnnotation <- Parsec.Common.optional $ Parsec.Common.try typeAnnotation

  (name, annotation) <- case optionalAnnotation >>= sanitizeTypeAnnotation of
    Just (name, annotation') -> return (name, Just annotation')
    Nothing -> Ipe.Parser.lexeme tldName >>= \name -> return (name, optionalAnnotation)

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
        Ipe.Grammar.topLevelDefinitionValue = value,
        Ipe.Grammar.topLevelDefinitionTypeAnnotation = annotation
      }

typeAnnotation :: Parser Ipe.Grammar.TypeAnnotation
typeAnnotation = do
  name <- Ipe.Parser.lexeme tldName

  Control.Monad.void (Ipe.Parser.symbol ":")

  firstType <- Ipe.Parser.lexeme $ Ipe.Parser.TypeDefinition.ipeType True

  otherTypes <-
    Parsec.Common.many
      ( Parsec.Common.try $
          Ipe.Parser.symbol "->"
            *> Ipe.Parser.lexeme (Ipe.Parser.TypeDefinition.ipeType True)
      )

  let (arguments, returnType) = unconsLast $ firstType : otherTypes

  return $
    Ipe.Grammar.TypeAnnotation
      { Ipe.Grammar.typeAnnotationName = name,
        Ipe.Grammar.typeAnnotationArguments = arguments,
        Ipe.Grammar.typeAnnotationReturnType = returnType
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

unconsLast :: [a] -> ([a], a)
unconsLast [] = error "unconsLast: empty list"
unconsLast [x] = ([], x)
unconsLast (x : xs) = (x : rest, lastElement)
  where
    (rest, lastElement) = unconsLast xs

sanitizeTypeAnnotation :: Ipe.Grammar.TypeAnnotation -> Maybe (Text, Ipe.Grammar.TypeAnnotation)
sanitizeTypeAnnotation annotation =
  case Ipe.Grammar.typeAnnotationReturnType annotation of
    Ipe.Grammar.ConcreteType concreteTypeName typeArgs ->
      case unconsLast typeArgs of
        (restOfTypeArgs, Ipe.Grammar.ParameterType lastTypeName) ->
          Just
            ( lastTypeName,
              annotation {Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType concreteTypeName restOfTypeArgs}
            )
        _ -> Nothing
    _ -> Nothing
