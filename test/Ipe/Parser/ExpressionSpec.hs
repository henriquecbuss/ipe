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
    recordSpec
    listSpec

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
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = [],
                Ipe.Grammar.functionCallOrValueName = "someValue",
                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                Ipe.Grammar.functionCallOrValueArguments = []
              }
          )

    it "should parse an imported value" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "SomeModule.someValue"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = ["SomeModule"],
                Ipe.Grammar.functionCallOrValueName = "someValue",
                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                Ipe.Grammar.functionCallOrValueArguments = []
              }
          )

    it "should parse a simple record accessor" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someValue.someField"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = [],
                Ipe.Grammar.functionCallOrValueName = "someValue",
                Ipe.Grammar.functionCallOrValueRecordAccessors = ["someField"],
                Ipe.Grammar.functionCallOrValueArguments = []
              }
          )

    it "should parse a simple record accessor from an imported value" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "SomeModule.Nested.someValue.someField.nested.field"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = ["SomeModule", "Nested"],
                Ipe.Grammar.functionCallOrValueName = "someValue",
                Ipe.Grammar.functionCallOrValueRecordAccessors = ["someField", "nested", "field"],
                Ipe.Grammar.functionCallOrValueArguments = []
              }
          )

    it "should parse a function with a value argument" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someFunction someValue"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = [],
                Ipe.Grammar.functionCallOrValueName = "someFunction",
                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                Ipe.Grammar.functionCallOrValueArguments =
                  [ Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someValue",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      )
                  ]
              }
          )

    it "should parse a function with multiple value arguments" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someFunction someValue1 someValue2 someValue3"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = [],
                Ipe.Grammar.functionCallOrValueName = "someFunction",
                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                Ipe.Grammar.functionCallOrValueArguments =
                  [ Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someValue1",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      ),
                    Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someValue2",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      ),
                    Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someValue3",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      )
                  ]
              }
          )

    it "should parse a function from a record with multiple value arguments" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someRecord.function someValue1 someValue2.field someValue3.nested.field"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = [],
                Ipe.Grammar.functionCallOrValueName = "someRecord",
                Ipe.Grammar.functionCallOrValueRecordAccessors = ["function"],
                Ipe.Grammar.functionCallOrValueArguments =
                  [ Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someValue1",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      ),
                    Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someValue2",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = ["field"],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      ),
                    Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someValue3",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = ["nested", "field"],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      )
                  ]
              }
          )

    it "should parse a function with a function with arguments as argument" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "someFunction (someOtherFunction someValue2 someValue3) someThirdFunction"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = [],
                Ipe.Grammar.functionCallOrValueName = "someFunction",
                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                Ipe.Grammar.functionCallOrValueArguments =
                  [ Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someOtherFunction",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments =
                              [ Ipe.Grammar.IpeFunctionCallOrValue
                                  ( Ipe.Grammar.FunctionCallOrValue
                                      { Ipe.Grammar.functionCallOrValuePath = [],
                                        Ipe.Grammar.functionCallOrValueName = "someValue2",
                                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                        Ipe.Grammar.functionCallOrValueArguments = []
                                      }
                                  ),
                                Ipe.Grammar.IpeFunctionCallOrValue
                                  ( Ipe.Grammar.FunctionCallOrValue
                                      { Ipe.Grammar.functionCallOrValuePath = [],
                                        Ipe.Grammar.functionCallOrValueName = "someValue3",
                                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                        Ipe.Grammar.functionCallOrValueArguments = []
                                      }
                                  )
                              ]
                          }
                      ),
                    Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someThirdFunction",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      )
                  ]
              }
          )

    it "should parse an imported function with an imported function with imported arguments as argument" $ do
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "Module1.Module2.someFunction\n\
        \  (Module1.Module2.someOtherFunction Module1.Module2.someValue2 someValue3)\n\
        \  Module3.someThirdFunction"
        `shouldParse` Ipe.Grammar.IpeFunctionCallOrValue
          ( Ipe.Grammar.FunctionCallOrValue
              { Ipe.Grammar.functionCallOrValuePath = ["Module1", "Module2"],
                Ipe.Grammar.functionCallOrValueName = "someFunction",
                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                Ipe.Grammar.functionCallOrValueArguments =
                  [ Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = ["Module1", "Module2"],
                            Ipe.Grammar.functionCallOrValueName = "someOtherFunction",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments =
                              [ Ipe.Grammar.IpeFunctionCallOrValue
                                  ( Ipe.Grammar.FunctionCallOrValue
                                      { Ipe.Grammar.functionCallOrValuePath = ["Module1", "Module2"],
                                        Ipe.Grammar.functionCallOrValueName = "someValue2",
                                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                        Ipe.Grammar.functionCallOrValueArguments = []
                                      }
                                  ),
                                Ipe.Grammar.IpeFunctionCallOrValue
                                  ( Ipe.Grammar.FunctionCallOrValue
                                      { Ipe.Grammar.functionCallOrValuePath = [],
                                        Ipe.Grammar.functionCallOrValueName = "someValue3",
                                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                        Ipe.Grammar.functionCallOrValueArguments = []
                                      }
                                  )
                              ]
                          }
                      ),
                    Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = ["Module3"],
                            Ipe.Grammar.functionCallOrValueName = "someThirdFunction",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      )
                  ]
              }
          )

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
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "someFunc",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments =
                      [ Ipe.Grammar.IpeFunctionCallOrValue
                          ( Ipe.Grammar.FunctionCallOrValue
                              { Ipe.Grammar.functionCallOrValuePath = [],
                                Ipe.Grammar.functionCallOrValueName = "firstArg",
                                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                Ipe.Grammar.functionCallOrValueArguments = []
                              }
                          )
                      ]
                  }
              )
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
                  ( Ipe.Grammar.IpeFunctionCallOrValue
                      ( Ipe.Grammar.FunctionCallOrValue
                          { Ipe.Grammar.functionCallOrValuePath = [],
                            Ipe.Grammar.functionCallOrValueName = "someFunc",
                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                            Ipe.Grammar.functionCallOrValueArguments = []
                          }
                      )
                  )
              )
              ( Ipe.Grammar.IpeFunctionCallOrValue
                  ( Ipe.Grammar.FunctionCallOrValue
                      { Ipe.Grammar.functionCallOrValuePath = ["Module1"],
                        Ipe.Grammar.functionCallOrValueName = "someOtherFunc",
                        Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                        Ipe.Grammar.functionCallOrValueArguments = []
                      }
                  )
              )
          )
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "yetAnotherFunc",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments =
                      [ Ipe.Grammar.IpeFunctionCallOrValue
                          ( Ipe.Grammar.FunctionCallOrValue
                              { Ipe.Grammar.functionCallOrValuePath = [],
                                Ipe.Grammar.functionCallOrValueName = "with_",
                                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                Ipe.Grammar.functionCallOrValueArguments = []
                              }
                          ),
                        Ipe.Grammar.IpeFunctionCallOrValue
                          ( Ipe.Grammar.FunctionCallOrValue
                              { Ipe.Grammar.functionCallOrValuePath = [],
                                Ipe.Grammar.functionCallOrValueName = "some",
                                Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                Ipe.Grammar.functionCallOrValueArguments =
                                  [ Ipe.Grammar.IpeFunctionCallOrValue
                                      ( Ipe.Grammar.FunctionCallOrValue
                                          { Ipe.Grammar.functionCallOrValuePath = ["Module2"],
                                            Ipe.Grammar.functionCallOrValueName = "args",
                                            Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                            Ipe.Grammar.functionCallOrValueArguments = []
                                          }
                                      )
                                  ]
                              }
                          ),
                        Ipe.Grammar.IpeNumber 42,
                        Ipe.Grammar.IpeString "hello"
                      ]
                  }
              )
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
              { Ipe.Grammar.functionBodyAttributions = [],
                Ipe.Grammar.functionReturn =
                  Ipe.Grammar.IpeBinaryOperation
                    Ipe.Grammar.Add
                    ( Ipe.Grammar.IpeFunctionCallOrValue
                        ( Ipe.Grammar.FunctionCallOrValue
                            { Ipe.Grammar.functionCallOrValuePath = [],
                              Ipe.Grammar.functionCallOrValueName = "x",
                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                              Ipe.Grammar.functionCallOrValueArguments = []
                            }
                        )
                    )
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
              { Ipe.Grammar.functionBodyAttributions =
                  [ ( "y",
                      Ipe.Grammar.IpeBinaryOperation
                        Ipe.Grammar.Add
                        ( Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "x",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = []
                                }
                            )
                        )
                        (Ipe.Grammar.IpeNumber 1)
                    ),
                    ( "z",
                      Ipe.Grammar.IpeFunctionCallOrValue
                        ( Ipe.Grammar.FunctionCallOrValue
                            { Ipe.Grammar.functionCallOrValuePath = [],
                              Ipe.Grammar.functionCallOrValueName = "complexOperation",
                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                              Ipe.Grammar.functionCallOrValueArguments =
                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                    ( Ipe.Grammar.FunctionCallOrValue
                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                          Ipe.Grammar.functionCallOrValueName = "y",
                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                          Ipe.Grammar.functionCallOrValueArguments = []
                                        }
                                    ),
                                  Ipe.Grammar.IpeFunctionCallOrValue
                                    ( Ipe.Grammar.FunctionCallOrValue
                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                          Ipe.Grammar.functionCallOrValueName = "someFunction",
                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                          Ipe.Grammar.functionCallOrValueArguments =
                                            [ Ipe.Grammar.IpeFunctionCallOrValue
                                                ( Ipe.Grammar.FunctionCallOrValue
                                                    { Ipe.Grammar.functionCallOrValuePath = [],
                                                      Ipe.Grammar.functionCallOrValueName = "x",
                                                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                      Ipe.Grammar.functionCallOrValueArguments = []
                                                    }
                                                ),
                                              Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Add (Ipe.Grammar.IpeNumber 2) (Ipe.Grammar.IpeNumber 3)
                                            ]
                                        }
                                    )
                                ]
                            }
                        )
                    )
                  ],
                Ipe.Grammar.functionReturn =
                  Ipe.Grammar.IpeBinaryOperation
                    Ipe.Grammar.Divide
                    ( Ipe.Grammar.IpeFunctionCallOrValue
                        ( Ipe.Grammar.FunctionCallOrValue
                            { Ipe.Grammar.functionCallOrValuePath = [],
                              Ipe.Grammar.functionCallOrValueName = "z",
                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                              Ipe.Grammar.functionCallOrValueArguments = []
                            }
                        )
                    )
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
              { Ipe.Grammar.functionBodyAttributions =
                  [ ( "a",
                      Ipe.Grammar.IpeBinaryOperation
                        Ipe.Grammar.Add
                        ( Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "x",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = []
                                }
                            )
                        )
                        (Ipe.Grammar.IpeNumber 1)
                    ),
                    ( "b",
                      Ipe.Grammar.IpeFunctionCallOrValue
                        ( Ipe.Grammar.FunctionCallOrValue
                            { Ipe.Grammar.functionCallOrValuePath = [],
                              Ipe.Grammar.functionCallOrValueName = "complexOperation",
                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                              Ipe.Grammar.functionCallOrValueArguments =
                                [ Ipe.Grammar.IpeFunctionCallOrValue
                                    ( Ipe.Grammar.FunctionCallOrValue
                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                          Ipe.Grammar.functionCallOrValueName = "y",
                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                          Ipe.Grammar.functionCallOrValueArguments = []
                                        }
                                    ),
                                  Ipe.Grammar.IpeFunctionCallOrValue
                                    ( Ipe.Grammar.FunctionCallOrValue
                                        { Ipe.Grammar.functionCallOrValuePath = [],
                                          Ipe.Grammar.functionCallOrValueName = "someFunction",
                                          Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                          Ipe.Grammar.functionCallOrValueArguments =
                                            [ Ipe.Grammar.IpeFunctionCallOrValue
                                                ( Ipe.Grammar.FunctionCallOrValue
                                                    { Ipe.Grammar.functionCallOrValuePath = [],
                                                      Ipe.Grammar.functionCallOrValueName = "x",
                                                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                                      Ipe.Grammar.functionCallOrValueArguments = []
                                                    }
                                                ),
                                              Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Add (Ipe.Grammar.IpeNumber 2) (Ipe.Grammar.IpeNumber 3)
                                            ]
                                        }
                                    )
                                ]
                            }
                        )
                    ),
                    ( "c",
                      Ipe.Grammar.IpeBinaryOperation
                        Ipe.Grammar.Multiply
                        ( Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "aFunction",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments =
                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "a",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments = []
                                            }
                                        ),
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "b",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                        )
                        ( Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "otherFunction",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments =
                                    [ Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "x",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments = []
                                            }
                                        ),
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "y",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments = []
                                            }
                                        ),
                                      Ipe.Grammar.IpeFunctionCallOrValue
                                        ( Ipe.Grammar.FunctionCallOrValue
                                            { Ipe.Grammar.functionCallOrValuePath = [],
                                              Ipe.Grammar.functionCallOrValueName = "z",
                                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                              Ipe.Grammar.functionCallOrValueArguments = []
                                            }
                                        )
                                    ]
                                }
                            )
                        )
                    )
                  ],
                Ipe.Grammar.functionReturn =
                  Ipe.Grammar.IpeBinaryOperation
                    Ipe.Grammar.Divide
                    ( Ipe.Grammar.IpeFunctionCallOrValue
                        ( Ipe.Grammar.FunctionCallOrValue
                            { Ipe.Grammar.functionCallOrValuePath = [],
                              Ipe.Grammar.functionCallOrValueName = "c",
                              Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                              Ipe.Grammar.functionCallOrValueArguments = []
                            }
                        )
                    )
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
        \ | _ -> 1"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [(Ipe.Grammar.IpeWildCardPattern, [], Ipe.Grammar.IpeNumber 1)]

    it "should parse a wildcard pattern with a single attribution" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | _ ->\n\
        \       y = 2;\n\
        \       1"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [(Ipe.Grammar.IpeWildCardPattern, [("y", Ipe.Grammar.IpeNumber 2)], Ipe.Grammar.IpeNumber 1)]

    it "should parse a single variable pattern" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | y -> 1"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [(Ipe.Grammar.IpeVariablePattern "y", [], Ipe.Grammar.IpeNumber 1)]

    it "should parse a single variable pattern with multiple attributions" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | y ->\n\
        \       x = 5;\n\
        \       z = 6 + 2;\n\
        \       1"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ ( Ipe.Grammar.IpeVariablePattern "y",
              [ ("x", Ipe.Grammar.IpeNumber 5),
                ("z", Ipe.Grammar.IpeBinaryOperation Ipe.Grammar.Add (Ipe.Grammar.IpeNumber 6) (Ipe.Grammar.IpeNumber 2))
              ],
              Ipe.Grammar.IpeNumber 1
            )
          ]

    it "should parse a simple custom type pattern" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | SomeConstructor -> 1"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [(Ipe.Grammar.IpeCustomTypePattern [] "SomeConstructor" [], [], Ipe.Grammar.IpeNumber 1)]

    it "should parse a custom type pattern that has some simple arguments" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | SomeConstructor -> 1\n\
        \ | OtherConstructor -> 2\n\
        \ | ThirdConstructor -> 3"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ (Ipe.Grammar.IpeCustomTypePattern [] "SomeConstructor" [], [], Ipe.Grammar.IpeNumber 1),
            (Ipe.Grammar.IpeCustomTypePattern [] "OtherConstructor" [], [], Ipe.Grammar.IpeNumber 2),
            (Ipe.Grammar.IpeCustomTypePattern [] "ThirdConstructor" [], [], Ipe.Grammar.IpeNumber 3)
          ]

    it "should parse a custom type pattern that has some simple arguments and a final one which is a variable" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | SomeConstructor -> 1\n\
        \ | OtherConstructor -> 2\n\
        \ | ThirdConstructor -> Imported.function 5 y (z 2)\n\
        \ | x -> 4"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ (Ipe.Grammar.IpeCustomTypePattern [] "SomeConstructor" [], [], Ipe.Grammar.IpeNumber 1),
            (Ipe.Grammar.IpeCustomTypePattern [] "OtherConstructor" [], [], Ipe.Grammar.IpeNumber 2),
            ( Ipe.Grammar.IpeCustomTypePattern [] "ThirdConstructor" [],
              [],
              Ipe.Grammar.IpeFunctionCallOrValue
                ( Ipe.Grammar.FunctionCallOrValue
                    { Ipe.Grammar.functionCallOrValuePath = ["Imported"],
                      Ipe.Grammar.functionCallOrValueName = "function",
                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                      Ipe.Grammar.functionCallOrValueArguments =
                        [ Ipe.Grammar.IpeNumber 5,
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "y",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = []
                                }
                            ),
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "z",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = [Ipe.Grammar.IpeNumber 2]
                                }
                            )
                        ]
                    }
                )
            ),
            (Ipe.Grammar.IpeVariablePattern "x", [], Ipe.Grammar.IpeNumber 4)
          ]

    it "should parse a custom type pattern that has some simple arguments and a final one which is a number" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | SomeConstructor -> 1\n\
        \ | OtherConstructor -> 2\n\
        \ | ThirdConstructor -> Imported.function 5 y (z 2)\n\
        \ | 1 -> 4"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ (Ipe.Grammar.IpeCustomTypePattern [] "SomeConstructor" [], [], Ipe.Grammar.IpeNumber 1),
            (Ipe.Grammar.IpeCustomTypePattern [] "OtherConstructor" [], [], Ipe.Grammar.IpeNumber 2),
            ( Ipe.Grammar.IpeCustomTypePattern [] "ThirdConstructor" [],
              [],
              Ipe.Grammar.IpeFunctionCallOrValue
                ( Ipe.Grammar.FunctionCallOrValue
                    { Ipe.Grammar.functionCallOrValuePath = ["Imported"],
                      Ipe.Grammar.functionCallOrValueName = "function",
                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                      Ipe.Grammar.functionCallOrValueArguments =
                        [ Ipe.Grammar.IpeNumber 5,
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "y",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = []
                                }
                            ),
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "z",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = [Ipe.Grammar.IpeNumber 2]
                                }
                            )
                        ]
                    }
                )
            ),
            (Ipe.Grammar.IpeLiteralNumberPattern 1, [], Ipe.Grammar.IpeNumber 4)
          ]

    it "should parse a custom type pattern that has some simple arguments and a final one which is a string" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | SomeConstructor -> 1\n\
        \ | OtherConstructor -> 2\n\
        \ | ThirdConstructor -> Imported.function 5 y (z 2)\n\
        \ | 'string' -> 4"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ (Ipe.Grammar.IpeCustomTypePattern [] "SomeConstructor" [], [], Ipe.Grammar.IpeNumber 1),
            (Ipe.Grammar.IpeCustomTypePattern [] "OtherConstructor" [], [], Ipe.Grammar.IpeNumber 2),
            ( Ipe.Grammar.IpeCustomTypePattern [] "ThirdConstructor" [],
              [],
              Ipe.Grammar.IpeFunctionCallOrValue
                ( Ipe.Grammar.FunctionCallOrValue
                    { Ipe.Grammar.functionCallOrValuePath = ["Imported"],
                      Ipe.Grammar.functionCallOrValueName = "function",
                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                      Ipe.Grammar.functionCallOrValueArguments =
                        [ Ipe.Grammar.IpeNumber 5,
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "y",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = []
                                }
                            ),
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "z",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = [Ipe.Grammar.IpeNumber 2]
                                }
                            )
                        ]
                    }
                )
            ),
            (Ipe.Grammar.IpeLiteralStringPattern "string", [], Ipe.Grammar.IpeNumber 4)
          ]

    it "should parse a custom type pattern that has some simple arguments and a final one which is a wildcard" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \ | SomeConstructor -> 1\n\
        \ | OtherConstructor -> 2\n\
        \ | ThirdConstructor -> Imported.function 5 y (z 2)\n\
        \ | _ -> 4"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ (Ipe.Grammar.IpeCustomTypePattern [] "SomeConstructor" [], [], Ipe.Grammar.IpeNumber 1),
            (Ipe.Grammar.IpeCustomTypePattern [] "OtherConstructor" [], [], Ipe.Grammar.IpeNumber 2),
            ( Ipe.Grammar.IpeCustomTypePattern [] "ThirdConstructor" [],
              [],
              Ipe.Grammar.IpeFunctionCallOrValue
                ( Ipe.Grammar.FunctionCallOrValue
                    { Ipe.Grammar.functionCallOrValuePath = ["Imported"],
                      Ipe.Grammar.functionCallOrValueName = "function",
                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                      Ipe.Grammar.functionCallOrValueArguments =
                        [ Ipe.Grammar.IpeNumber 5,
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "y",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = []
                                }
                            ),
                          Ipe.Grammar.IpeFunctionCallOrValue
                            ( Ipe.Grammar.FunctionCallOrValue
                                { Ipe.Grammar.functionCallOrValuePath = [],
                                  Ipe.Grammar.functionCallOrValueName = "z",
                                  Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                                  Ipe.Grammar.functionCallOrValueArguments = [Ipe.Grammar.IpeNumber 2]
                                }
                            )
                        ]
                    }
                )
            ),
            (Ipe.Grammar.IpeWildCardPattern, [], Ipe.Grammar.IpeNumber 4)
          ]

    it "should parse a custom type pattern that could be confused with an expression" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \  | SomeConstructor ->\n\
        \     a\n\
        \  | OtherConstructor arg ->\n\
        \     2"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ ( Ipe.Grammar.IpeCustomTypePattern [] "SomeConstructor" [],
              [],
              Ipe.Grammar.IpeFunctionCallOrValue
                ( Ipe.Grammar.FunctionCallOrValue
                    { Ipe.Grammar.functionCallOrValuePath = [],
                      Ipe.Grammar.functionCallOrValueName = "a",
                      Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                      Ipe.Grammar.functionCallOrValueArguments = []
                    }
                )
            ),
            ( Ipe.Grammar.IpeCustomTypePattern
                []
                "OtherConstructor"
                [ "arg"
                ],
              [],
              Ipe.Grammar.IpeNumber 2
            )
          ]

    it "should parse a list pattern" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "match x with\n\
        \  | ['blog', 'edit', blogId] ->\n\
        \     0\n\
        \  | [] ->\n\
        \     0"
        `shouldParse` Ipe.Grammar.IpeMatch
          ( Ipe.Grammar.IpeFunctionCallOrValue
              ( Ipe.Grammar.FunctionCallOrValue
                  { Ipe.Grammar.functionCallOrValuePath = [],
                    Ipe.Grammar.functionCallOrValueName = "x",
                    Ipe.Grammar.functionCallOrValueRecordAccessors = [],
                    Ipe.Grammar.functionCallOrValueArguments = []
                  }
              )
          )
          [ ( Ipe.Grammar.IpeLiteralListPattern
                [ Ipe.Grammar.IpeLiteralStringPattern "blog",
                  Ipe.Grammar.IpeLiteralStringPattern "edit",
                  Ipe.Grammar.IpeVariablePattern "blogId"
                ],
              [],
              Ipe.Grammar.IpeNumber 0
            ),
            ( Ipe.Grammar.IpeLiteralListPattern [],
              [],
              Ipe.Grammar.IpeNumber 0
            )
          ]

recordSpec :: Spec
recordSpec =
  context "when parsing records" $ do
    it "should parse a simple flat record" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "{ x = 5, y = 'abc' }"
        `shouldParse` Ipe.Grammar.IpeRecord
          [ ("x", Ipe.Grammar.IpeNumber 5),
            ("y", Ipe.Grammar.IpeString "abc")
          ]

    it "should parse a complex nested record" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "{ a = 5\n\
        \, b = { c = { d = d\n\
        \            , e = 4\n\
        \            }\n\
        \      , f = 'abcdef'\n\
        \      , g = {}\n\
        \      }\n\
        \, h = 'abc'\n\
        \, k = { l = 1 }\n\
        \}"
        `shouldParse` Ipe.Grammar.IpeRecord
          [ ("a", Ipe.Grammar.IpeNumber 5),
            ( "b",
              Ipe.Grammar.IpeRecord
                [ ( "c",
                    Ipe.Grammar.IpeRecord
                      [ ("d", Ipe.Grammar.IpeFunctionCallOrValue $ Ipe.Grammar.FunctionCallOrValue [] "d" [] []),
                        ("e", Ipe.Grammar.IpeNumber 4)
                      ]
                  ),
                  ("f", Ipe.Grammar.IpeString "abcdef"),
                  ("g", Ipe.Grammar.IpeRecord [])
                ]
            ),
            ("h", Ipe.Grammar.IpeString "abc"),
            ("k", Ipe.Grammar.IpeRecord [("l", Ipe.Grammar.IpeNumber 1)])
          ]

listSpec :: Spec
listSpec =
  context "when parsing lists" $ do
    it "should parse an empty list" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "[]"
        `shouldParse` Ipe.Grammar.IpeList []

    it "should parse a simple list" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "[1, a, 1 + b]"
        `shouldParse` Ipe.Grammar.IpeList
          [ Ipe.Grammar.IpeNumber 1,
            Ipe.Grammar.IpeFunctionCallOrValue $ Ipe.Grammar.FunctionCallOrValue [] "a" [] [],
            Ipe.Grammar.IpeBinaryOperation
              Ipe.Grammar.Add
              (Ipe.Grammar.IpeNumber 1)
              (Ipe.Grammar.IpeFunctionCallOrValue $ Ipe.Grammar.FunctionCallOrValue [] "b" [] [])
          ]

    it "should parse a complex nested list" $
      Parsec.Common.parse
        Ipe.Parser.Expression.parser
        ""
        "[1, [], 2, [3, 4]]"
        `shouldParse` Ipe.Grammar.IpeList
          [ Ipe.Grammar.IpeNumber 1,
            Ipe.Grammar.IpeList [],
            Ipe.Grammar.IpeNumber 2,
            Ipe.Grammar.IpeList
              [ Ipe.Grammar.IpeNumber 3,
                Ipe.Grammar.IpeNumber 4
              ]
          ]
