{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.FunctionSpec (spec) where

import qualified Ipe.Grammar
import qualified Ipe.Parser.Function
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec = describe "the function parser" $ do
  it "should parse a function with a single argument and a single return" $ do
    Parsec.Common.parse
      Ipe.Parser.Function.parser
      ""
      "\\x -> x + 1"
      `shouldParse` Ipe.Grammar.IpeFunction
        { Ipe.Grammar.arguments = ["x"],
          Ipe.Grammar.functionBody =
            Ipe.Grammar.IpeFunctionBody
              { Ipe.Grammar.attributions = [],
                Ipe.Grammar.functionReturn =
                  Ipe.Grammar.IpeBinaryOperation
                    Ipe.Grammar.Add
                    (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                    (Ipe.Grammar.IpeNumber 1)
              }
        }

  it "should parse a function with a single argument and a few attributions" $ do
    Parsec.Common.parse
      Ipe.Parser.Function.parser
      ""
      "\\x ->\n\
      \  y = x + 1;\n\
      \  z = complexOperation y\n\
      \    (someFunction x (2 + 3));\n\
      \  z / 3"
      `shouldParse` Ipe.Grammar.IpeFunction
        { Ipe.Grammar.arguments = ["x"],
          Ipe.Grammar.functionBody =
            Ipe.Grammar.IpeFunctionBody
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
        }

  it "should parse a function with more arguments and a few attributions" $ do
    Parsec.Common.parse
      Ipe.Parser.Function.parser
      ""
      "\\x y z ->\n\
      \  a = x + 1;\n\
      \  b = complexOperation y\n\
      \    (someFunction x (2 + 3));\n\
      \  c = (aFunction a b) * (otherFunction x y z);\n\
      \  c / 3"
      `shouldParse` Ipe.Grammar.IpeFunction
        { Ipe.Grammar.arguments = ["x", "y", "z"],
          Ipe.Grammar.functionBody =
            Ipe.Grammar.IpeFunctionBody
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
        }
