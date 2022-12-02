{-# LANGUAGE OverloadedStrings #-}

module Ipe.ParserSpec (spec) where

import qualified Data.Text as T
import qualified Ipe.Parser
import Test.Hspec
import Test.Hspec.Megaparsec (err, etoks, initialState, shouldFailWith, shouldParse, succeedsLeaving, utoks)
import Test.Hspec.QuickCheck
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

spec :: Spec
spec = do
  lexemeSpec
  symbolSpec
  symbolWithNoBlockCommentsSpec
  spaceSpec
  spaceWithNoBlockCommentsSpec
  docCommentSpec

lexemeSpec :: Spec
lexemeSpec =
  describe "the lexeme parser" $ do
    it "should parse a lexeme" $
      Parsec.Common.parse
        (Ipe.Parser.lexeme (Parsec.Char.string "abc"))
        ""
        "abc"
        `shouldParse` "abc"

    it "should parse a lexeme with whitespace after" $
      Parsec.Common.runParser'
        (Ipe.Parser.lexeme (Parsec.Char.string "abc"))
        (initialState "abc /* a block comment */  \n\t\t // a line comment  ")
        `succeedsLeaving` ""

    it "should not consume any other input" $
      Parsec.Common.runParser'
        (Ipe.Parser.lexeme (Parsec.Char.string "abc"))
        (initialState "abcdef  \n\t\t   ")
        `succeedsLeaving` "def  \n\t\t   "

    prop "should parse any lexeme" $ do
      \stringS ->
        let s = T.pack (stringS :: String)
         in Parsec.Common.parse
              (Ipe.Parser.lexeme (Parsec.Char.string s))
              ""
              s
              `shouldParse` s

symbolSpec :: Spec
symbolSpec =
  describe "the symbol parser" $ do
    it "should parse a symbol" $
      Parsec.Common.parse
        (Ipe.Parser.symbol "abc")
        ""
        "abc"
        `shouldParse` "abc"

    it "should parse a symbol with whitespace after" $
      Parsec.Common.runParser'
        (Ipe.Parser.symbol "abc")
        (initialState "abc /* a block comment */  \n\t\t // a line comment  ")
        `succeedsLeaving` ""

    it "should not consume any other input" $
      Parsec.Common.runParser'
        (Ipe.Parser.symbol "abc")
        (initialState "abcdef  \n\t\t   ")
        `succeedsLeaving` "def  \n\t\t   "

    prop "should parse any symbol" $ do
      \stringS ->
        let s = T.pack (stringS :: String)
         in Parsec.Common.parse
              (Ipe.Parser.symbol s)
              ""
              s
              `shouldParse` s

symbolWithNoBlockCommentsSpec :: Spec
symbolWithNoBlockCommentsSpec =
  describe "the symbolWithNoBlockComments parser" $ do
    it "should parse a symbol" $
      Parsec.Common.parse
        (Ipe.Parser.symbolWithNoBlockComments "abc")
        ""
        "abc"
        `shouldParse` "abc"

    it "should parse a symbol with whitespace after" $
      Parsec.Common.runParser'
        (Ipe.Parser.symbolWithNoBlockComments "abc")
        (initialState "abc /* a block comment */  \n\t\t // a line comment  ")
        `succeedsLeaving` "/* a block comment */  \n\t\t // a line comment  "

    it "should not consume any other input" $
      Parsec.Common.runParser'
        (Ipe.Parser.symbolWithNoBlockComments "abc")
        (initialState "abcdef  \n\t\t   ")
        `succeedsLeaving` "def  \n\t\t   "

    prop "should parse any symbol" $ do
      \stringS ->
        let s = T.pack (stringS :: String)
         in Parsec.Common.parse
              (Ipe.Parser.symbolWithNoBlockComments s)
              ""
              s
              `shouldParse` s

spaceSpec :: Spec
spaceSpec =
  describe "the space parser" $ do
    it "should consume a bunch of whitespace" $
      Parsec.Common.runParser'
        Ipe.Parser.space
        (initialState " \n\t \t\t")
        `succeedsLeaving` ""

    it "should consume comments" $
      Parsec.Common.runParser'
        Ipe.Parser.space
        (initialState " \n\t // some comment \t\n\t/* some block comment */ \n \t")
        `succeedsLeaving` ""

    it "should succeed but not consume any input" $
      Parsec.Common.runParser'
        Ipe.Parser.space
        (initialState "abc \n\t \t\t")
        `succeedsLeaving` "abc \n\t \t\t"

spaceWithNoBlockCommentsSpec :: Spec
spaceWithNoBlockCommentsSpec =
  describe "the spaceWithNoBlockComments parser" $ do
    it "should consume a bunch of whitespace" $
      Parsec.Common.runParser'
        Ipe.Parser.spaceWithNoBlockComments
        (initialState " \n\t \t\t")
        `succeedsLeaving` ""

    it "should consume line comments" $
      Parsec.Common.runParser'
        Ipe.Parser.spaceWithNoBlockComments
        (initialState " \n\t // some comment \t\n\t")
        `succeedsLeaving` ""

    it "should not consume block comments" $
      Parsec.Common.runParser'
        Ipe.Parser.spaceWithNoBlockComments
        (initialState " \n\t // some comment \t\n\t/* some block comment */ \n \t")
        `succeedsLeaving` "/* some block comment */ \n \t"

    it "should succeed but not consume any input" $
      Parsec.Common.runParser'
        Ipe.Parser.spaceWithNoBlockComments
        (initialState "abc \n\t \t\t")
        `succeedsLeaving` "abc \n\t \t\t"

docCommentSpec :: Spec
docCommentSpec =
  describe "the docComment parser" $ do
    it "should parse a doc comment" $
      Parsec.Common.parse
        Ipe.Parser.docComment
        ""
        "/*| some doc comment */"
        `shouldParse` "some doc comment "

    it "should parse a multiline doc comment" $
      Parsec.Common.parse
        Ipe.Parser.docComment
        ""
        "/*| some doc comment \nwith\nmultiple\nlines\n\t\n*/"
        `shouldParse` "some doc comment \nwith\nmultiple\nlines\n\t\n"

    it "should fail if the comment is not a doc comment" $
      Parsec.Common.parse
        Ipe.Parser.docComment
        ""
        "/* some comment */"
        `shouldFailWith` err 0 (utoks "/* " <> etoks "/*|")

    it "should fail with other input" $
      Parsec.Common.parse
        Ipe.Parser.docComment
        ""
        "x = 5"
        `shouldFailWith` err 0 (utoks "x =" <> etoks "/*|")
