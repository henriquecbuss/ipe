{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.TypeDefinitionSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Ipe.Grammar
import qualified Ipe.Parser.TypeDefinition
import Test.Hspec
import Test.Hspec.Megaparsec (elabel, err, etoks, failsLeaving, initialState, shouldFailOn, shouldFailWith, shouldFailWithM, shouldParse, ueof, utok, utoks)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec = do
  parserSpec

parserSpec :: Spec
parserSpec =
  describe "the type definition parser" $ do
    typeAlias

typeAlias :: Spec
typeAlias =
  context "when parsing a type alias" $ do
    typeAliasWithConcreteTypes
    typeAliasWithParameterTypes
    typeAliasWithRecord

    context "the definition" $ do
      it "should fail with typo in alias" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alas SomeType = Number"
          `shouldFailWith` err 5 (utoks "alas " <> etoks "alias")

      it "should fail with a dot in the name" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          "type alias Some.Type = Number"
          `shouldFailOn` "."

      it "should fail with no `=`" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          "type alias SomeType Number"
          `shouldFailOn` "N"

      it "should fail with lowercase names" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alias someType = Number"
          `shouldFailWith` err 11 (utok 's' <> elabel "a type definition name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

      it "should fail with invalid character in the middle of name" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          "type alias S!omeType = Number"
          `shouldFailOn` "!"

      it "should fail with invalid type" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alias SomeType = !Number"
          `shouldFailWith` err 22 (utok '!' <> elabel "a type, which can start with an uppercase or lowercase letter, or a record, with fields inside curly brackets (`{` and `}`)")

typeAliasWithConcreteTypes :: Spec
typeAliasWithConcreteTypes =
  context "with concrete types" $ do
    it "can parse a simple type" $
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = Number"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
          }

    it "can parse a simple type with parentheses" $
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = (Number)"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
          }

    it "can parse a type with doc comments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "/*| Some doc comment \n\
        \*/\n\n\
        \type alias SomeType = Number"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Just "Some doc comment \n",
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
          }

    it "can parse a type with arguments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType a b = Number"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = ["a", "b"],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
          }

    it "can parse a type with parameters" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = List Number"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "List" [Ipe.Grammar.ConcreteType "Number" []]
          }

    it "can parse a type with parameters and parentheses" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = (List (Number))"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "List" [Ipe.Grammar.ConcreteType "Number" []]
          }

    it "can't parse with missing closing parenthesis" $ do
      Parsec.Common.runParser'
        Ipe.Parser.TypeDefinition.parser
        (initialState "type alias SomeType = (List (Number)")
        `failsLeaving` ""

    it "can parse a very deeply nested type" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = Some.Very.Very.Very.Very.Deeply.Nested.Type"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Some.Very.Very.Very.Very.Deeply.Nested.Type" []
          }

typeAliasWithParameterTypes :: Spec
typeAliasWithParameterTypes =
  context "with parameter types" $ do
    it "can parse a simple type" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = a"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ParameterType "a"
          }

    it "can parse a type with doc comments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "/*| Some doc comment \n\
        \*/\n\n\
        \type alias SomeType = a"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = [],
            Ipe.Grammar.typeAliasDefinitionDocComment = Just "Some doc comment \n",
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ParameterType "a"
          }

    it "can parse a type with arguments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType a b = a"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
            Ipe.Grammar.typeAliasDefinitionParameters = ["a", "b"],
            Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
            Ipe.Grammar.typeAliasType = Ipe.Grammar.ParameterType "a"
          }

typeAliasWithRecord :: Spec
typeAliasWithRecord =
  context "when parsing a type alias" $ do
    context "with record types" $ do
      it "can parse a simple type" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alias SomeType =\n\
          \  { someNumber : Number\n\
          \  , someString : String \n\
          \  }"
          `shouldParse` Ipe.Grammar.TypeAliasDefinition
            { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
              Ipe.Grammar.typeAliasDefinitionParameters = [],
              Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
              Ipe.Grammar.typeAliasType =
                Ipe.Grammar.RecordType
                  ( Map.fromList
                      [ ("someNumber", Ipe.Grammar.ConcreteType "Number" []),
                        ("someString", Ipe.Grammar.ConcreteType "String" [])
                      ]
                  )
            }
      it "can parse a complex type with arguments and nested parameters" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "/*| Some doc comment */\n\
          \ type alias SomeType a =\n\
          \  { someNumber : Number\n\
          \  , someString : String \n\
          \  , someComplexType: Result (Maybe (List String)) (Dict String (Set User.Id))\n\
          \  }"
          `shouldParse` Ipe.Grammar.TypeAliasDefinition
            { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
              Ipe.Grammar.typeAliasDefinitionParameters = ["a"],
              Ipe.Grammar.typeAliasDefinitionDocComment = Just "Some doc comment ",
              Ipe.Grammar.typeAliasType =
                Ipe.Grammar.RecordType
                  ( Map.fromList
                      [ ("someNumber", Ipe.Grammar.ConcreteType "Number" []),
                        ("someString", Ipe.Grammar.ConcreteType "String" []),
                        ( "someComplexType",
                          Ipe.Grammar.ConcreteType
                            "Result"
                            [ Ipe.Grammar.ConcreteType "Maybe" [Ipe.Grammar.ConcreteType "List" [Ipe.Grammar.ConcreteType "String" []]],
                              Ipe.Grammar.ConcreteType
                                "Dict"
                                [ Ipe.Grammar.ConcreteType "String" [],
                                  Ipe.Grammar.ConcreteType "Set" [Ipe.Grammar.ConcreteType "User.Id" []]
                                ]
                            ]
                        )
                      ]
                  )
            }
