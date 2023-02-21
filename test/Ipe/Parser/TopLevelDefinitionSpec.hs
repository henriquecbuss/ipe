{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.TopLevelDefinitionSpec (spec) where

import qualified Ipe.Grammar
import qualified Ipe.Parser.TopLevelDefinition
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import qualified Text.Megaparsec as Parsec.Common

spec :: Spec
spec = describe "the top level definition parser" $ do
  it "should parse a function with a single argument and a single return" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelFunction = \\x -> x + 1"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelFunction",
          Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelFunction
              ( Ipe.Grammar.IpeFunction
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
              )
        }

  it "should parse a top level expression" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelExpression = x + 1"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelExpression",
          Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelExpression
              ( Ipe.Grammar.IpeBinaryOperation
                  Ipe.Grammar.Add
                  (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                  (Ipe.Grammar.IpeNumber 1)
              )
        }

  it "should parse a function with a single argument and a single return and a doc comment" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "/*| A top level function that adds 1 to its argument */\n\
      \topLevelFunction = \\x -> x + 1"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelFunction",
          Ipe.Grammar.topLevelDefinitionDocComment = Just "A top level function that adds 1 to its argument ",
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelFunction
              ( Ipe.Grammar.IpeFunction
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
              )
        }

  it "should parse a top level expression" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "/*| A top level expression that equals x + 1 */\n\
      \topLevelExpression = x + 1"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelExpression",
          Ipe.Grammar.topLevelDefinitionDocComment = Just "A top level expression that equals x + 1 ",
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelExpression
              ( Ipe.Grammar.IpeBinaryOperation
                  Ipe.Grammar.Add
                  (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                  (Ipe.Grammar.IpeNumber 1)
              )
        }
