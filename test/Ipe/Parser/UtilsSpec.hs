{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.UtilsSpec (spec) where

import qualified Data.Text as T
import qualified Ipe.Parser.Utils as Ipe.Parser
import Test.Hspec
import Test.Hspec.Megaparsec (elabel, err, etoks, initialState, shouldFailWith, shouldParse, succeedsLeaving, utok, utoks)
import Test.Hspec.QuickCheck
import qualified Text.Megaparsec as Parsec.Common
import qualified Text.Megaparsec.Char as Parsec.Char

spec :: Spec
spec = do
  moduleNameSpec
  lexemeSpec
  hlexemeSpec
  symbolSpec
  symbolWithNoBlockCommentsSpec
  spaceSpec
  hspaceSpec
  spaceWithNoBlockCommentsSpec
  docCommentSpec

moduleNameSpec :: Spec
moduleNameSpec =
  describe "the moduleName parser" $ do
    it "should parse a valid module name" $
      Parsec.Common.parse
        Ipe.Parser.moduleName
        ""
        "SomeModule"
        `shouldParse` ([], "SomeModule")

    it "should parse a module name with dots" $
      Parsec.Common.parse
        Ipe.Parser.moduleName
        ""
        "Some.Module"
        `shouldParse` (["Some"], "Module")

    it "should fail on a module that starts with a lower case letter" $
      Parsec.Common.parse
        Ipe.Parser.moduleName
        ""
        "someModule"
        `shouldFailWith` err 0 (utok 's' <> elabel "uppercase letter")

    it "should fail with a lower case letter after a dot" $
      Parsec.Common.parse
        Ipe.Parser.moduleName
        ""
        "Some.module"
        `shouldFailWith` err 5 (utok 'm' <> elabel "uppercase letter")

    it "should fail with a lower case letter after many dots" $
      Parsec.Common.parse
        Ipe.Parser.moduleName
        ""
        "Some.Very.Very.Very.Very.Deeply.nested.Module"
        `shouldFailWith` err 32 (utok 'n' <> elabel "uppercase letter")

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

hlexemeSpec :: Spec
hlexemeSpec =
  describe "the hlexeme parser" $ do
    it "should parse a lexeme" $
      Parsec.Common.parse
        (Ipe.Parser.hlexeme (Parsec.Char.string "abc"))
        ""
        "abc"
        `shouldParse` "abc"

    it "should parse a lexeme with whitespace after" $
      Parsec.Common.runParser'
        (Ipe.Parser.hlexeme (Parsec.Char.string "abc"))
        (initialState "abc /* a block comment */  \t\t // a line comment  ")
        `succeedsLeaving` ""

    it "should not consume anything after a line break" $
      Parsec.Common.runParser'
        (Ipe.Parser.hlexeme (Parsec.Char.string "abc"))
        (initialState "abc /* a block comment */  \n\t // a line comment  ")
        `succeedsLeaving` "\n\t // a line comment  "

    it "should not consume anything after a line break inside a block comment" $
      Parsec.Common.runParser'
        (Ipe.Parser.hlexeme (Parsec.Char.string "abc"))
        (initialState "abc /* a block comment with\na line break */  \n\t // a line comment  ")
        `succeedsLeaving` "a line break */  \n\t // a line comment  "

    it "should not consume any other input" $
      Parsec.Common.runParser'
        (Ipe.Parser.hlexeme (Parsec.Char.string "abc"))
        (initialState "abcdef  \n\t\t   ")
        `succeedsLeaving` "def  \n\t\t   "

    prop "should parse any lexeme" $ do
      \stringS ->
        let s = T.pack (stringS :: String)
         in Parsec.Common.parse
              (Ipe.Parser.hlexeme (Parsec.Char.string s))
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

hspaceSpec :: Spec
hspaceSpec =
  describe "the hspace parser" $ do
    it "should consume a bunch of whitespace" $
      Parsec.Common.runParser'
        Ipe.Parser.hspace
        (initialState " \t \t\t")
        `succeedsLeaving` ""

    it "should stop consuming on line breaks" $
      Parsec.Common.runParser'
        Ipe.Parser.hspace
        (initialState " \t \n\t\t")
        `succeedsLeaving` "\n\t\t"

    it "should consume comments" $
      Parsec.Common.runParser'
        Ipe.Parser.hspace
        (initialState " \t // some comment \t\t/* some block comment */  \t")
        `succeedsLeaving` ""

    it "should consume comments and stop on line breaks inside block comments" $
      Parsec.Common.runParser'
        Ipe.Parser.hspace
        (initialState " \t // some comment \t\t/* some block\n comment */  \t")
        `succeedsLeaving` "\n comment */  \t"

    it "should succeed but not consume any input" $
      Parsec.Common.runParser'
        Ipe.Parser.hspace
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
        "/|* some doc comment */"
        `shouldParse` "some doc comment "

    it "should parse a multiline doc comment" $
      Parsec.Common.parse
        Ipe.Parser.docComment
        ""
        "/|* some doc comment \nwith\nmultiple\nlines\n\t\n*/"
        `shouldParse` "some doc comment \nwith\nmultiple\nlines\n\t\n"

    it "should fail if the comment is not a doc comment" $
      Parsec.Common.parse
        Ipe.Parser.docComment
        ""
        "/* some comment */"
        `shouldFailWith` err 0 (utoks "/* " <> etoks "/|*")

    it "should fail with other input" $
      Parsec.Common.parse
        Ipe.Parser.docComment
        ""
        "x = 5"
        `shouldFailWith` err 0 (utoks "x =" <> etoks "/|*")
