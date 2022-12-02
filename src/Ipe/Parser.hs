{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser
  ( Parser,
    moduleName,
    lexeme,
    hlexeme,
    symbol,
    symbolWithNoBlockComments,
    space,
    hspace,
    spaceWithNoBlockComments,
    docComment,
  )
where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char
import qualified Text.Megaparsec.Char.Lexer as Parsec.Lexer

-- | The main parser type for Ipe
type Parser = Parsec.Common.Parsec Void Text

-- | Parse a module name. Module names start with an upper case letter, and
-- contain only letters, numbers, `.` or `_`. Letters after a `.` must be upper
-- case as well
moduleName :: Parser Text
moduleName = do
  firstChar <- Parsec.Char.upperChar
  rest <-
    Parsec.Common.many
      ( Parsec.Common.choice
          [ Parsec.Char.alphaNumChar,
            Parsec.Char.char '_'
          ]
          <?> "the rest of the module name, which can be any combination of letters, numbers, `.` or `_`"
      )

  nestedModules <- Parsec.Common.many (Parsec.Char.char '.' *> moduleName)

  return $
    T.concat
      ( Data.List.intersperse
          "."
          (T.pack (firstChar : rest) : nestedModules)
      )

-- | Consume all whitespace, line comments and block comments right after a parser
lexeme :: Parser a -> Parser a
lexeme = Parsec.Lexer.lexeme space

-- | Consume all whitespace, line comments and block comments right after a parser,
-- but not line breaks
hlexeme :: Parser a -> Parser a
hlexeme = Parsec.Lexer.lexeme hspace

-- | Consume all whitespace, line comments and block comments right after a text
symbol :: Text -> Parser Text
symbol = Parsec.Lexer.symbol space

-- | Consume all whitespace and line comments right after a text
symbolWithNoBlockComments :: Text -> Parser Text
symbolWithNoBlockComments = Parsec.Lexer.symbol spaceWithNoBlockComments

-- | Consume all whitespace, line comments and block comments
space :: Parser ()
space =
  Parsec.Lexer.space
    Parsec.Char.space1
    (Parsec.Lexer.skipLineComment singleLineCommentStart)
    (Parsec.Lexer.skipBlockComment blockCommentStart blockCommentEnd)

-- | Consume all whitespace, line comments and block comments, but not line breaks
hspace :: Parser ()
hspace =
  Parsec.Lexer.space
    Parsec.Char.hspace1
    (Parsec.Lexer.skipLineComment singleLineCommentStart)
    (hSkipBlockComment blockCommentStart blockCommentEnd)

-- | Consume all whitespace and line comments
spaceWithNoBlockComments :: Parser ()
spaceWithNoBlockComments =
  Parsec.Lexer.space
    Parsec.Char.space1
    (Parsec.Lexer.skipLineComment singleLineCommentStart)
    Control.Applicative.empty

-- | Consume block comments, but stop on line breaks
hSkipBlockComment :: Text -> Text -> Parser ()
hSkipBlockComment start end = do
  Control.Monad.void (Parsec.Char.string start)

  Control.Monad.void $
    Parsec.Common.manyTill
      Parsec.Common.anySingle
      (Parsec.Char.eol <|> Parsec.Char.string end)

-- | Consume a doc comment and retrieve it's text
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
