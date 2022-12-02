{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser
  ( Parser,
    lexeme,
    symbol,
    symbolWithNoBlockComments,
    space,
    spaceWithNoBlockComments,
    docComment,
    -- expression,
    -- assign,
  )
where

import qualified Control.Applicative
import qualified Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char
import qualified Text.Megaparsec.Char.Lexer as Parsec.Lexer

-- | The main parser type for Ipe
type Parser = Parsec.Common.Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = Parsec.Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = Parsec.Lexer.symbol space

symbolWithNoBlockComments :: Text -> Parser Text
symbolWithNoBlockComments = Parsec.Lexer.symbol spaceWithNoBlockComments

space :: Parser ()
space =
  Parsec.Lexer.space
    Parsec.Char.space1
    (Parsec.Lexer.skipLineComment singleLineCommentStart)
    (Parsec.Lexer.skipBlockComment blockCommentStart blockCommentEnd)

spaceWithNoBlockComments :: Parser ()
spaceWithNoBlockComments =
  Parsec.Lexer.space
    Parsec.Char.space1
    (Parsec.Lexer.skipLineComment singleLineCommentStart)
    Control.Applicative.empty

docComment :: Parser Text
docComment = do
  Control.Monad.void (symbol docCommentStart)

  comment <-
    Parsec.Common.manyTill
      Parsec.Common.anySingle
      (symbol docCommentEnd <?> "the end of the doc comment (with `*/`)")

  return $ T.pack comment

singleLineCommentStart :: Text
singleLineCommentStart = "//"

blockCommentStart :: Text
blockCommentStart = "/*"

blockCommentEnd :: Text
blockCommentEnd = "*/"

docCommentStart :: Text
docCommentStart = "/*|"

docCommentEnd :: Text
docCommentEnd = "*/"
