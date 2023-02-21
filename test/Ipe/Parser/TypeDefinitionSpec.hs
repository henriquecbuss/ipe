{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.TypeDefinitionSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Ipe.Grammar
import qualified Ipe.Parser.TypeDefinition
import Test.Hspec
import Test.Hspec.Megaparsec (elabel, err, etoks, failsLeaving, initialState, shouldFailWith, shouldParse, ueof, utok, utoks)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec = do
  parserSpec

parserSpec :: Spec
parserSpec =
  describe "the type definition parser" $ do
    typeAlias
    typeUnion
    typeOpaque

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
          `shouldFailWith` err 5 (utoks "alas S" <> etoks "alias" <> etoks "opaque" <> etoks "union")

      it "should fail with a dot in the name" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alias Some.Type = Number"
          `shouldFailWith` err
            15
            ( utok '.'
                <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
                <> elabel "an `=`, followed by the actual type definition"
                <> elabel "the rest of the type definition name, which can be any combination of letters, numbers or `_`"
            )

      it "should fail with no `=`" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alias SomeType Number"
          `shouldFailWith` err
            20
            ( utok 'N'
                <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
                <> elabel "an `=`, followed by the actual type definition"
            )

      it "should fail with lowercase names" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alias someType = Number"
          `shouldFailWith` err 11 (utok 's' <> elabel "a type definition name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

      it "should fail with invalid character in the middle of name" $ do
        Parsec.Common.parse
          Ipe.Parser.TypeDefinition.parser
          ""
          "type alias S!omeType = Number"
          `shouldFailWith` err
            12
            ( utok '!'
                <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
                <> elabel "an `=`, followed by the actual type definition"
                <> elabel "the rest of the type definition name, which can be any combination of letters, numbers or `_`"
            )

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
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
              }
          )

    it "can parse a simple type with parentheses" $
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = (Number)"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
              }
          )

    it "can parse a type with doc comments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "/*| Some doc comment \n\
        \*/\n\n\
        \type alias SomeType = Number"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Just "Some doc comment \n",
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
              }
          )

    it "can parse a type with arguments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType a b = Number"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = ["a", "b"],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Number" []
              }
          )

    it "can parse a type with parameters" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = List Number"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "List" [Ipe.Grammar.ConcreteType "Number" []]
              }
          )

    it "can parse a type with parameters and parentheses" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = (List (Number))"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "List" [Ipe.Grammar.ConcreteType "Number" []]
              }
          )

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
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ConcreteType "Some.Very.Very.Very.Very.Deeply.Nested.Type" []
              }
          )

typeAliasWithParameterTypes :: Spec
typeAliasWithParameterTypes =
  context "with parameter types" $ do
    it "can parse a simple type" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType = a"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ParameterType "a"
              }
          )

    it "can parse a type with doc comments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "/*| Some doc comment \n\
        \*/\n\n\
        \type alias SomeType = a"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = [],
                Ipe.Grammar.typeAliasDefinitionDocComment = Just "Some doc comment \n",
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ParameterType "a"
              }
          )

    it "can parse a type with arguments" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type alias SomeType a b = a"
        `shouldParse` Ipe.Grammar.TypeAliasDefinition
          ( Ipe.Grammar.TypeAlias
              { Ipe.Grammar.typeAliasDefinitionName = "SomeType",
                Ipe.Grammar.typeAliasDefinitionParameters = ["a", "b"],
                Ipe.Grammar.typeAliasDefinitionDocComment = Nothing,
                Ipe.Grammar.typeAliasType = Ipe.Grammar.ParameterType "a"
              }
          )

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
            ( Ipe.Grammar.TypeAlias
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
            )
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
            ( Ipe.Grammar.TypeAlias
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
            )

typeUnion :: Spec
typeUnion =
  context "when parsing a type union" $ do
    it "should parse a single constructor" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = | Constructor"
        `shouldParse` Ipe.Grammar.TypeUnionDefinition
          ( Ipe.Grammar.TypeUnion
              { Ipe.Grammar.typeUnionDefinitionName = "SomeType",
                Ipe.Grammar.typeUnionDefinitionParameters = [],
                Ipe.Grammar.typeUnionDefinitionDocComment = Nothing,
                Ipe.Grammar.typeUnionDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs = []
                      }
                  ]
              }
          )

    it "should parse a single with a simple argument" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = | Constructor Number"
        `shouldParse` Ipe.Grammar.TypeUnionDefinition
          ( Ipe.Grammar.TypeUnion
              { Ipe.Grammar.typeUnionDefinitionName = "SomeType",
                Ipe.Grammar.typeUnionDefinitionParameters = [],
                Ipe.Grammar.typeUnionDefinitionDocComment = Nothing,
                Ipe.Grammar.typeUnionDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Number" []
                          ]
                      }
                  ]
              }
          )

    it "should parse a single with a complex argument" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType =\n\
        \  | Constructor \n\
        \      Number\n\
        \      (OneOf\n\
        \        a\n\
        \        { someType : String\n\
        \        , someOtherType : b\n\
        \        , anEmptyRecord : {}\n\
        \        }\n\
        \        String\n\
        \      )\n\
        \      c"
        `shouldParse` Ipe.Grammar.TypeUnionDefinition
          ( Ipe.Grammar.TypeUnion
              { Ipe.Grammar.typeUnionDefinitionName = "SomeType",
                Ipe.Grammar.typeUnionDefinitionParameters = [],
                Ipe.Grammar.typeUnionDefinitionDocComment = Nothing,
                Ipe.Grammar.typeUnionDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Number" [],
                            Ipe.Grammar.ConcreteType
                              "OneOf"
                              [ Ipe.Grammar.ParameterType "a",
                                Ipe.Grammar.RecordType
                                  ( Map.fromList
                                      [ ("someType", Ipe.Grammar.ConcreteType "String" []),
                                        ("someOtherType", Ipe.Grammar.ParameterType "b"),
                                        ("anEmptyRecord", Ipe.Grammar.RecordType Map.empty)
                                      ]
                                  ),
                                Ipe.Grammar.ConcreteType
                                  "String"
                                  []
                              ],
                            Ipe.Grammar.ParameterType "c"
                          ]
                      }
                  ]
              }
          )

    it "should parse when there are many constructors" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType =\n\
        \  | Constructor Number\n\
        \  | AnotherConstructor a {}\n\
        \  | YetAnotherConstructor (Maybe (List String))"
        `shouldParse` Ipe.Grammar.TypeUnionDefinition
          ( Ipe.Grammar.TypeUnion
              { Ipe.Grammar.typeUnionDefinitionName = "SomeType",
                Ipe.Grammar.typeUnionDefinitionParameters = [],
                Ipe.Grammar.typeUnionDefinitionDocComment = Nothing,
                Ipe.Grammar.typeUnionDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Number" []
                          ]
                      },
                    Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "AnotherConstructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ParameterType "a",
                            Ipe.Grammar.RecordType Map.empty
                          ]
                      },
                    Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "YetAnotherConstructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType
                              "Maybe"
                              [ Ipe.Grammar.ConcreteType "List" [Ipe.Grammar.ConcreteType "String" []]
                              ]
                          ]
                      }
                  ]
              }
          )

    it "should fail when missing `|` with a single constructor" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = Constructor Number"
        `shouldFailWith` err
          22
          ( utok 'C'
              <> elabel "a `|`, followed by a constructor name"
              <> elabel "a doc comment, starting with `/*|` and ending with `*/`"
          )

    it "should fail when there are no constructors" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = | "
        `shouldFailWith` err 24 (ueof <> elabel "a type constructor name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should fail with a lowercase constructor name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = | constructor Number"
        `shouldFailWith` err 24 (utok 'c' <> elabel "a type constructor name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should fail with typo in union" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type unon SomeType = | Constructor Number"
        `shouldFailWith` err 5 (utoks "unon S" <> etoks "alias" <> etoks "opaque" <> etoks "union")

    it "should fail with a dot in the name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union Some.Type = | Constructor Number"
        `shouldFailWith` err
          15
          ( utok '.'
              <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
              <> elabel "the actual type definition"
              <> elabel "the rest of the type definition name, which can be any combination of letters, numbers or `_`"
          )

    it "should fail with no `=`" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType | Constructor Number"
        `shouldFailWith` err
          20
          ( utok '|'
              <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
              <> elabel "the actual type definition"
          )

    it "should fail with lowercase names" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union someType = | Constructor Number"
        `shouldFailWith` err 11 (utok 's' <> elabel "a type definition name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should fail with invalid character in the middle of name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union S!omeType = | Constructor Number"
        `shouldFailWith` err
          12
          ( utok '!'
              <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
              <> elabel "the actual type definition"
              <> elabel "the rest of the type definition name, which can be any combination of letters, numbers or `_`"
          )

    it "should fail with invalid type" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = | !Constructor Number"
        `shouldFailWith` err 24 (utok '!' <> elabel "a type constructor name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should parse imported types" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = | Constructor Some.Other.Type"
        `shouldParse` Ipe.Grammar.TypeUnionDefinition
          ( Ipe.Grammar.TypeUnion
              { Ipe.Grammar.typeUnionDefinitionName = "SomeType",
                Ipe.Grammar.typeUnionDefinitionParameters = [],
                Ipe.Grammar.typeUnionDefinitionDocComment = Nothing,
                Ipe.Grammar.typeUnionDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Some.Other.Type" []
                          ]
                      }
                  ]
              }
          )

    it "shouldn't allow dots in the constructor name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type union SomeType = | Some.Constructor Number"
        `shouldParse` Ipe.Grammar.TypeUnionDefinition
          ( Ipe.Grammar.TypeUnion
              { Ipe.Grammar.typeUnionDefinitionName = "SomeType",
                Ipe.Grammar.typeUnionDefinitionParameters = [],
                Ipe.Grammar.typeUnionDefinitionDocComment = Nothing,
                Ipe.Grammar.typeUnionDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Some",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs = []
                      }
                  ]
              }
          )

typeOpaque :: Spec
typeOpaque =
  context "when parsing a type opaque" $ do
    it "should parse a single constructor" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = | Constructor"
        `shouldParse` Ipe.Grammar.TypeOpaqueDefinition
          ( Ipe.Grammar.TypeOpaque
              { Ipe.Grammar.typeOpaqueDefinitionName = "SomeType",
                Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                Ipe.Grammar.typeOpaqueDefinitionDocComment = Nothing,
                Ipe.Grammar.typeOpaqueDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs = []
                      }
                  ]
              }
          )

    it "should parse a single with a simple argument" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = | Constructor Number"
        `shouldParse` Ipe.Grammar.TypeOpaqueDefinition
          ( Ipe.Grammar.TypeOpaque
              { Ipe.Grammar.typeOpaqueDefinitionName = "SomeType",
                Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                Ipe.Grammar.typeOpaqueDefinitionDocComment = Nothing,
                Ipe.Grammar.typeOpaqueDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Number" []
                          ]
                      }
                  ]
              }
          )

    it "should parse a single with a complex argument" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType =\n\
        \  | Constructor \n\
        \      Number\n\
        \      (OneOf\n\
        \        a\n\
        \        { someType : String\n\
        \        , someOtherType : b\n\
        \        , anEmptyRecord : {}\n\
        \        }\n\
        \        String\n\
        \      )\n\
        \      c"
        `shouldParse` Ipe.Grammar.TypeOpaqueDefinition
          ( Ipe.Grammar.TypeOpaque
              { Ipe.Grammar.typeOpaqueDefinitionName = "SomeType",
                Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                Ipe.Grammar.typeOpaqueDefinitionDocComment = Nothing,
                Ipe.Grammar.typeOpaqueDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Number" [],
                            Ipe.Grammar.ConcreteType
                              "OneOf"
                              [ Ipe.Grammar.ParameterType "a",
                                Ipe.Grammar.RecordType
                                  ( Map.fromList
                                      [ ("someType", Ipe.Grammar.ConcreteType "String" []),
                                        ("someOtherType", Ipe.Grammar.ParameterType "b"),
                                        ("anEmptyRecord", Ipe.Grammar.RecordType Map.empty)
                                      ]
                                  ),
                                Ipe.Grammar.ConcreteType
                                  "String"
                                  []
                              ],
                            Ipe.Grammar.ParameterType "c"
                          ]
                      }
                  ]
              }
          )

    it "should parse when there are many constructors" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType =\n\
        \  | Constructor Number\n\
        \  | AnotherConstructor a {}\n\
        \  | YetAnotherConstructor (Maybe (List String))"
        `shouldParse` Ipe.Grammar.TypeOpaqueDefinition
          ( Ipe.Grammar.TypeOpaque
              { Ipe.Grammar.typeOpaqueDefinitionName = "SomeType",
                Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                Ipe.Grammar.typeOpaqueDefinitionDocComment = Nothing,
                Ipe.Grammar.typeOpaqueDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Number" []
                          ]
                      },
                    Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "AnotherConstructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ParameterType "a",
                            Ipe.Grammar.RecordType Map.empty
                          ]
                      },
                    Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "YetAnotherConstructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType
                              "Maybe"
                              [ Ipe.Grammar.ConcreteType "List" [Ipe.Grammar.ConcreteType "String" []]
                              ]
                          ]
                      }
                  ]
              }
          )

    it "should fail when missing `|` with a single constructor" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = Constructor Number"
        `shouldFailWith` err
          23
          ( utok 'C'
              <> elabel "a `|`, followed by a constructor name"
              <> elabel "a doc comment, starting with `/*|` and ending with `*/`"
          )

    it "should fail when there are no constructors" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = | "
        `shouldFailWith` err 25 (ueof <> elabel "a type constructor name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should fail with a lowercase constructor name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = | constructor Number"
        `shouldFailWith` err 25 (utok 'c' <> elabel "a type constructor name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should fail with typo in opaque" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaqe SomeType = | Constructor Number"
        `shouldFailWith` err 5 (utoks "opaqe " <> etoks "alias" <> etoks "opaque" <> etoks "union")

    it "should fail with a dot in the name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque Some.Type = | Constructor Number"
        `shouldFailWith` err
          16
          ( utok '.'
              <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
              <> elabel "the actual type definition"
              <> elabel "the rest of the type definition name, which can be any combination of letters, numbers or `_`"
          )

    it "should fail with no `=`" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType | Constructor Number"
        `shouldFailWith` err
          21
          ( utok '|'
              <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
              <> elabel "the actual type definition"
          )

    it "should fail with lowercase names" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque someType = | Constructor Number"
        `shouldFailWith` err 12 (utok 's' <> elabel "a type definition name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should fail with invalid character in the middle of name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque S!omeType = | Constructor Number"
        `shouldFailWith` err
          13
          ( utok '!'
              <> elabel "a type definition parameter, which must start with a lowercase letter, and followed by any combination of numbers, letters or `_`"
              <> elabel "the actual type definition"
              <> elabel "the rest of the type definition name, which can be any combination of letters, numbers or `_`"
          )

    it "should fail with invalid type" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = | !Constructor Number"
        `shouldFailWith` err 25 (utok '!' <> elabel "a type constructor name, which must start with an uppercase letter, and followed by any combination of numbers, letters or `_`")

    it "should parse imported types" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = | Constructor Some.Other.Type"
        `shouldParse` Ipe.Grammar.TypeOpaqueDefinition
          ( Ipe.Grammar.TypeOpaque
              { Ipe.Grammar.typeOpaqueDefinitionName = "SomeType",
                Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                Ipe.Grammar.typeOpaqueDefinitionDocComment = Nothing,
                Ipe.Grammar.typeOpaqueDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Constructor",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs =
                          [ Ipe.Grammar.ConcreteType "Some.Other.Type" []
                          ]
                      }
                  ]
              }
          )

    it "shouldn't allow dots in the constructor name" $ do
      Parsec.Common.parse
        Ipe.Parser.TypeDefinition.parser
        ""
        "type opaque SomeType = | Some.Constructor Number"
        `shouldParse` Ipe.Grammar.TypeOpaqueDefinition
          ( Ipe.Grammar.TypeOpaque
              { Ipe.Grammar.typeOpaqueDefinitionName = "SomeType",
                Ipe.Grammar.typeOpaqueDefinitionParameters = [],
                Ipe.Grammar.typeOpaqueDefinitionDocComment = Nothing,
                Ipe.Grammar.typeOpaqueDefinitionConstructors =
                  [ Ipe.Grammar.CustomTypeConstructor
                      { Ipe.Grammar.customTypeConstructorName = "Some",
                        Ipe.Grammar.customTypeConstructorDocComment = Nothing,
                        Ipe.Grammar.customTypeConstructorArgs = []
                      }
                  ]
              }
          )
