{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.ExpressionSpec (spec) where

import qualified Ipe.Grammar
import qualified Ipe.Parser.Expression
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec =
  describe "the expression parser" $ do
    numberSpec
    stringSpec
    functionCallOrValueSpec
    binaryOperatorSpec
    lambdaFunctionSpec
    patternMatchSpec

numberSpec :: Spec
numberSpec = do
  context "when parsing numbers" $ do
    it "should parse a regular integer" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "5"
        `shouldParse` Ipe.Grammar.IpeNumber 5

    it "should parse a regular float" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "3.14"
        `shouldParse` Ipe.Grammar.IpeNumber 3.14

    it "should parse a negative integer" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "-5"
        `shouldParse` Ipe.Grammar.IpeNumber (-5)

    it "should parse a negative float" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "-3.14"
        `shouldParse` Ipe.Grammar.IpeNumber (-3.14)

stringSpec :: Spec
stringSpec =
  context "when parsing strings" $ do
    it "should parse an empty string" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "''"
        `shouldParse` Ipe.Grammar.IpeString ""

    it "should parse a string with a single character" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "'a'"
        `shouldParse` Ipe.Grammar.IpeString "a"

    it "should parse a string with a few characters" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "'abcdefg'"
        `shouldParse` Ipe.Grammar.IpeString "abcdefg"

    it "should parse a string with emojis" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "'🚀🔥'"
        `shouldParse` Ipe.Grammar.IpeString "🚀🔥"

    it "should stop parsing when encountering a single quote" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "'that's an awesome language!'"
        `shouldParse` Ipe.Grammar.IpeString "that"

    it "should parse a string with escaped quotes" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "'that\\'s an awesome language!'"
        `shouldParse` Ipe.Grammar.IpeString "that's an awesome language!"

functionCallOrValueSpec :: Spec
functionCallOrValueSpec =
  context "when parsing a function call or value" $ do
    it "should parse a simple value" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someValue"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue "someValue" []

    it "should parse an imported value" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "SomeModule.someValue"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue "SomeModule.someValue" []

    it "should parse a function with a value argument" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someFunction someValue"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          "someFunction"
          [ Ipe.Grammar.IpeFunctionCallOrValue "someValue" []
          ]

    it "should parse a function with multiple value arguments" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someFunction someValue1 someValue2 someValue3"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          "someFunction"
          [ Ipe.Grammar.IpeFunctionCallOrValue "someValue1" [],
            Ipe.Grammar.IpeFunctionCallOrValue "someValue2" [],
            Ipe.Grammar.IpeFunctionCallOrValue "someValue3" []
          ]

    it "should parse a function with a function with arguments as argument" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someFunction (someOtherFunction someValue2 someValue3) someThirdFunction"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          "someFunction"
          [ Ipe.Grammar.IpeFunctionCallOrValue
              "someOtherFunction"
              [ Ipe.Grammar.IpeFunctionCallOrValue "someValue2" [],
                Ipe.Grammar.IpeFunctionCallOrValue "someValue3" []
              ],
            Ipe.Grammar.IpeFunctionCallOrValue "someThirdFunction" []
          ]

    it "should parse an imported function with an imported function with imported arguments as argument" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "Module1.Module2.someFunction\n\
        \  (Module1.Module2.someOtherFunction Module1.Module2.someValue2 someValue3)\n\
        \  Module3.someThirdFunction"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          "Module1.Module2.someFunction"
          [ Ipe.Grammar.IpeFunctionCallOrValue
              "Module1.Module2.someOtherFunction"
              [ Ipe.Grammar.IpeFunctionCallOrValue "Module1.Module2.someValue2" [],
                Ipe.Grammar.IpeFunctionCallOrValue "someValue3" []
              ],
            Ipe.Grammar.IpeFunctionCallOrValue "Module3.someThirdFunction" []
          ]

binaryOperatorSpec :: Spec
binaryOperatorSpec =
  context "when parsing binary operators" $ do
    it "should parse a number + a number" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "1 + 1"
        `shouldParse` Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Add (Ipe.Grammar.IpeNumber 1) (Ipe.Grammar.IpeNumber 1)

    it "should parse a number - a number" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "1 - 1"
        `shouldParse` Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Subtract (Ipe.Grammar.IpeNumber 1) (Ipe.Grammar.IpeNumber 1)

    it "should parse a number + a number + a number" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "1 + 1 + 1"
        `shouldParse` Ipe.Grammar.IpeBinaryOperation
          Ipe.Grammar.Add
          (Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Add (Ipe.Grammar.IpeNumber 1) (Ipe.Grammar.IpeNumber 1))
          (Ipe.Grammar.IpeNumber 1)

    it "should respect order of operations" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "1 + 2 * 3 / 4 ^ 2 - 1 + 3 * -1"
        `shouldParse` Ipe.Grammar.IpeBinaryOperation
          Ipe.Grammar.Add
          ( Ipe.Grammar.IpeBinaryOperation
              Ipe.Grammar.Subtract
              ( Ipe.Grammar.IpeBinaryOperation
                  Ipe.Grammar.Add
                  (Ipe.Grammar.IpeNumber 1)
                  ( Ipe.Grammar.IpeBinaryOperation
                      Ipe.Grammar.Divide
                      ( Ipe.Grammar.IpeBinaryOperation
                          Ipe.Grammar.Multiply
                          (Ipe.Grammar.IpeNumber 2)
                          (Ipe.Grammar.IpeNumber 3)
                      )
                      ( Ipe.Grammar.IpeBinaryOperation
                          Ipe.Grammar.Exponentiation
                          (Ipe.Grammar.IpeNumber 4)
                          (Ipe.Grammar.IpeNumber 2)
                      )
                  )
              )
              (Ipe.Grammar.IpeNumber 1)
          )
          ( Ipe.Grammar.IpeBinaryOperation
              Ipe.Grammar.Multiply
              (Ipe.Grammar.IpeNumber 3)
              (Ipe.Grammar.IpeNumber (-1))
          )

    it "should parse math operations piped into some function" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "1 + 2 * 3 / 3 - 4\n  |> someFunc firstArg"
        `shouldParse` Ipe.Grammar.IpeBinaryOperation
          Ipe.Grammar.PipeRight
          ( Ipe.Grammar.IpeBinaryOperation
              Ipe.Grammar.Subtract
              ( Ipe.Grammar.IpeBinaryOperation
                  Ipe.Grammar.Add
                  (Ipe.Grammar.IpeNumber 1)
                  ( Ipe.Grammar.IpeBinaryOperation
                      Ipe.Grammar.Divide
                      ( Ipe.Grammar.IpeBinaryOperation
                          Ipe.Grammar.Multiply
                          (Ipe.Grammar.IpeNumber 2)
                          (Ipe.Grammar.IpeNumber 3)
                      )
                      (Ipe.Grammar.IpeNumber 3)
                  )
              )
              (Ipe.Grammar.IpeNumber 4)
          )
          ( Ipe.Grammar.IpeFunctionCallOrValue
              "someFunc"
              [ Ipe.Grammar.IpeFunctionCallOrValue "firstArg" []
              ]
          )

    it "should parse long pipelines" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "5 + 2\n\
        \  |> someFunc\n\
        \  |> Module1.someOtherFunc\n\
        \  |> yetAnotherFunc with_ (some Module2.args) 42 'hello'"
        `shouldParse` Ipe.Grammar.IpeBinaryOperation
          Ipe.Grammar.PipeRight
          ( Ipe.Grammar.IpeBinaryOperation
              Ipe.Grammar.PipeRight
              ( Ipe.Grammar.IpeBinaryOperation
                  Ipe.Grammar.PipeRight
                  ( Ipe.Grammar.IpeBinaryOperation
                      Ipe.Grammar.Add
                      (Ipe.Grammar.IpeNumber 5)
                      (Ipe.Grammar.IpeNumber 2)
                  )
                  (Ipe.Grammar.IpeFunctionCallOrValue "someFunc" [])
              )
              (Ipe.Grammar.IpeFunctionCallOrValue "Module1.someOtherFunc" [])
          )
          ( Ipe.Grammar.IpeFunctionCallOrValue
              "yetAnotherFunc"
              [ Ipe.Grammar.IpeFunctionCallOrValue "with_" [],
                Ipe.Grammar.IpeFunctionCallOrValue "some" [Ipe.Grammar.IpeFunctionCallOrValue "Module2.args" []],
                Ipe.Grammar.IpeNumber 42,
                Ipe.Grammar.IpeString "hello"
              ]
          )

lambdaFunctionSpec :: Spec
lambdaFunctionSpec =
  context "when parsing lambda functions" $ do
    it "should parse a function with a single argument and a single return" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "\\x -> x + 1"
        `shouldParse` Ipe.Grammar.IpeFunction
          ["x"]
          ( Ipe.Grammar.IpeFunctionBody
              { Ipe.Grammar.attributions = [],
                Ipe.Grammar.functionReturn =
                  Ipe.Grammar.IpeBinaryOperation
                    Ipe.Grammar.Add
                    (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                    (Ipe.Grammar.IpeNumber 1)
              }
          )

    it "should parse a function with a single argument and a few attributions" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "\\x ->\n\
        \  y = x + 1;\n\
        \  z = complexOperation y\n\
        \    (someFunction x (2 + 3));\n\
        \  z / 3"
        `shouldParse` Ipe.Grammar.IpeFunction
          ["x"]
          ( Ipe.Grammar.IpeFunctionBody
              { Ipe.Grammar.attributions =
                  [ ( "y",
                      Ipe.Grammar.IpeBinaryOperation
                        Ipe.Grammar.Add
                        (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                        (Ipe.Grammar.IpeNumber 1)
                    ),
                    ( "z",
                      Ipe.Grammar.IpeFunctionCallOrValue
                        "complexOperation"
                        [ Ipe.Grammar.IpeFunctionCallOrValue "y" [],
                          Ipe.Grammar.IpeFunctionCallOrValue
                            "someFunction"
                            [ Ipe.Grammar.IpeFunctionCallOrValue "x" [],
                              Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Add (Ipe.Grammar.IpeNumber 2) (Ipe.Grammar.IpeNumber 3)
                            ]
                        ]
                    )
                  ],
                Ipe.Grammar.functionReturn =
                  Ipe.Grammar.IpeBinaryOperation
                    Ipe.Grammar.Divide
                    (Ipe.Grammar.IpeFunctionCallOrValue "z" [])
                    (Ipe.Grammar.IpeNumber 3)
              }
          )

    it "should parse a function with more arguments and a few attributions" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "\\x y z ->\n\
        \  a = x + 1;\n\
        \  b = complexOperation y\n\
        \    (someFunction x (2 + 3));\n\
        \  c = (aFunction a b) * (otherFunction x y z);\n\
        \  c / 3"
        `shouldParse` Ipe.Grammar.IpeFunction
          ["x", "y", "z"]
          ( Ipe.Grammar.IpeFunctionBody
              { Ipe.Grammar.attributions =
                  [ ( "a",
                      Ipe.Grammar.IpeBinaryOperation
                        Ipe.Grammar.Add
                        (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                        (Ipe.Grammar.IpeNumber 1)
                    ),
                    ( "b",
                      Ipe.Grammar.IpeFunctionCallOrValue
                        "complexOperation"
                        [ Ipe.Grammar.IpeFunctionCallOrValue "y" [],
                          Ipe.Grammar.IpeFunctionCallOrValue
                            "someFunction"
                            [ Ipe.Grammar.IpeFunctionCallOrValue "x" [],
                              Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Add (Ipe.Grammar.IpeNumber 2) (Ipe.Grammar.IpeNumber 3)
                            ]
                        ]
                    ),
                    ( "c",
                      Ipe.Grammar.IpeBinaryOperation
                        Ipe.Grammar.Multiply
                        (Ipe.Grammar.IpeFunctionCallOrValue "aFunction" [Ipe.Grammar.IpeFunctionCallOrValue "a" [], Ipe.Grammar.IpeFunctionCallOrValue "b" []])
                        (Ipe.Grammar.IpeFunctionCallOrValue "otherFunction" [Ipe.Grammar.IpeFunctionCallOrValue "x" [], Ipe.Grammar.IpeFunctionCallOrValue "y" [], Ipe.Grammar.IpeFunctionCallOrValue "z" []])
                    )
                  ],
                Ipe.Grammar.functionReturn =
                  Ipe.Grammar.IpeBinaryOperation
                    Ipe.Grammar.Divide
                    (Ipe.Grammar.IpeFunctionCallOrValue "c" [])
                    (Ipe.Grammar.IpeNumber 3)
              }
          )

patternMatchSpec :: Spec
patternMatchSpec = do
  context "when parsing pattern matching" $ do
    it "should parse a wildcard pattern" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ _ -> 1"
        `shouldParse` Ipe.Grammar.IpeMatch
          (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
          [(Ipe.Grammar.IpeWildCardPattern, Ipe.Grammar.IpeNumber 1)]

    it "should parse a single variable pattern" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ y -> 1"
        `shouldParse` Ipe.Grammar.IpeMatch
          (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
          [(Ipe.Grammar.IpeVariablePattern "y", Ipe.Grammar.IpeNumber 1)]

    it "should parse a simple custom type pattern" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ SomeConstructor -> 1"
        `shouldParse` Ipe.Grammar.IpeMatch
          (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
          [(Ipe.Grammar.IpeCustomTypePattern "SomeConstructor" [], Ipe.Grammar.IpeNumber 1)]

    it "should parse a custom type pattern that has some simple arguments" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ SomeConstructor -> 1\n\
        \ OtherConstructor -> 2\n\
        \ ThirdConstructor -> 3"
        `shouldParse` Ipe.Grammar.IpeMatch
          (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
          [ (Ipe.Grammar.IpeCustomTypePattern "SomeConstructor" [], Ipe.Grammar.IpeNumber 1),
            (Ipe.Grammar.IpeCustomTypePattern "OtherConstructor" [], Ipe.Grammar.IpeNumber 2),
            (Ipe.Grammar.IpeCustomTypePattern "ThirdConstructor" [], Ipe.Grammar.IpeNumber 3)
          ]

    it "should parse a custom type pattern that has some complex arguments" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ SomeConstructor a 5 -> a\n\
        \ OtherConstructor (NestedConstructor 'abc') -> 2\n\
        \ Imported.ThirdConstructor (Level1 (Level2 x)) 5 'abc' -> 3 + x"
        `shouldParse` Ipe.Grammar.IpeMatch
          (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
          [ ( Ipe.Grammar.IpeCustomTypePattern
                "SomeConstructor"
                [ Ipe.Grammar.IpeVariablePattern "a",
                  Ipe.Grammar.IpeLiteralNumberPattern 5
                ],
              Ipe.Grammar.IpeFunctionCallOrValue "a" []
            ),
            ( Ipe.Grammar.IpeCustomTypePattern
                "OtherConstructor"
                [Ipe.Grammar.IpeCustomTypePattern "NestedConstructor" [Ipe.Grammar.IpeLiteralStringPattern "abc"]],
              Ipe.Grammar.IpeNumber 2
            ),
            ( Ipe.Grammar.IpeCustomTypePattern
                "Imported.ThirdConstructor"
                [ Ipe.Grammar.IpeCustomTypePattern
                    "Level1"
                    [ Ipe.Grammar.IpeCustomTypePattern
                        "Level2"
                        [Ipe.Grammar.IpeVariablePattern "x"]
                    ],
                  Ipe.Grammar.IpeLiteralNumberPattern 5,
                  Ipe.Grammar.IpeLiteralStringPattern "abc"
                ],
              Ipe.Grammar.IpeBinaryOperation
                Ipe.Grammar.Add
                (Ipe.Grammar.IpeNumber 3)
                (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
            )
          ]
