{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.ModuleDefinitionSpec (spec) where

import qualified Ipe.Grammar
import qualified Ipe.Parser.ModuleDefinition
import Test.Hspec
import Test.Hspec.Megaparsec (elabel, err, etoks, shouldFailWith, shouldParse, ueof, utok, utoks)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec = do
  parserSpec

parserSpec :: Spec
parserSpec =
  describe "the module definition parser" $ do
    it "should parse a valid module definition" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module SomeModule exports [ someFunction, otherFunction ]"
        `shouldParse` Ipe.Grammar.ModuleDefinition
          { Ipe.Grammar.moduleDefinitionPath = [],
            Ipe.Grammar.moduleDefinitionName = "SomeModule",
            Ipe.Grammar.exportedDefinitions = ["someFunction", "otherFunction"],
            Ipe.Grammar.moduleDocComment = Nothing
          }

    it "should parse a valid module definition with a doc comment" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module SomeModule exports [ someFunction, otherFunction ]\n\
        \/|*\n\
        \This is a doc comment!\n\
        \Here's another line\n\
        \*/"
        `shouldParse` Ipe.Grammar.ModuleDefinition
          { Ipe.Grammar.moduleDefinitionPath = [],
            Ipe.Grammar.moduleDefinitionName = "SomeModule",
            Ipe.Grammar.exportedDefinitions = ["someFunction", "otherFunction"],
            Ipe.Grammar.moduleDocComment = Just "This is a doc comment!\nHere's another line\n"
          }

    it "should allow dots in the module name" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module Some.Module exports [ someFunction, otherFunction ]"
        `shouldParse` Ipe.Grammar.ModuleDefinition
          { Ipe.Grammar.moduleDefinitionPath = ["Some"],
            Ipe.Grammar.moduleDefinitionName = "Module",
            Ipe.Grammar.exportedDefinitions = ["someFunction", "otherFunction"],
            Ipe.Grammar.moduleDocComment = Nothing
          }

    it "should fail on an empty exports list" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module SomeModule exports []"
        `shouldFailWith` err 27 (utok ']' <> elabel "a top level definition name. Top level definition names must start with a letter (lowercase for functions or uppercase for types), and contain only letters, numbers, `.` or `_`")

    it "should fail on a lowercase module name" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module someModule exports [ someFunction, otherFunction ]"
        `shouldFailWith` err 7 (utok 's' <> elabel "a module name. Module names must start with an upper case letter, and contain only letters, numbers, `.` or `_`")

    it "should fail when forgetting `module`" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "SomeModule exports [ someFunction, otherFunction ]"
        `shouldFailWith` err 0 (utoks "SomeMo" <> etoks "module")

    it "should fail when forgetting `exports`" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module SomeModule [ someFunction, otherFunction ]"
        `shouldFailWith` err 18 (utoks "[ someF" <> elabel "the `exports` keyword, followed by a list of exported definitions")

    it "should fail when forgetting the export list" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module SomeModule exports"
        `shouldFailWith` err 25 (ueof <> elabel "the list of exported definitions. It must be a list of comma-separated items, surrounded by square brackets (`[` and `]`). For example: `module SomeModule exports [ someFunction, someOtherFunction ]")

    it "should fail when forgetting the module name" $
      Parsec.Common.parse
        Ipe.Parser.ModuleDefinition.parser
        ""
        "module exports [ someFunction, otherFunction ]"
        `shouldFailWith` err 7 (utok 'e' <> elabel "a module name. Module names must start with an upper case letter, and contain only letters, numbers, `.` or `_`")
