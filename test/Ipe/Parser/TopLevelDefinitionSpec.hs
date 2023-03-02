{-# LANGUAGE OverloadedStrings #-}

module Ipe.Parser.TopLevelDefinitionSpec (spec) where

import qualified Data.Map.Strict as Map
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
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation = Nothing
        }

  it "should parse a function with two arguments and a single return" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelFunction = \\x y -> x + y"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelFunction",
          Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelFunction
              ( Ipe.Grammar.IpeFunction
                  { Ipe.Grammar.arguments = ["x", "y"],
                    Ipe.Grammar.functionBody =
                      Ipe.Grammar.IpeFunctionBody
                        { Ipe.Grammar.attributions = [],
                          Ipe.Grammar.functionReturn =
                            Ipe.Grammar.IpeBinaryOperation
                              Ipe.Grammar.Add
                              (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                              (Ipe.Grammar.IpeFunctionCallOrValue "y" [])
                        }
                  }
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation = Nothing
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
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation = Nothing
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
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation = Nothing
        }

  it "should parse a top level expression with doc comment" $ do
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
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation = Nothing
        }

  it "should parse a function with a single argument and a single return with type annotation" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelFunction : Number -> Number\n\
      \topLevelFunction = \\x -> x + 1"
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
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation =
            Just
              ( Ipe.Grammar.TypeAnnotation
                  { Ipe.Grammar.typeAnnotationName = "topLevelFunction",
                    Ipe.Grammar.typeAnnotationArguments = [Ipe.Grammar.ConcreteType "Number" []],
                    Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType "Number" []
                  }
              )
        }

  it "should parse a function with two arguments and a single return and type annotation" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelFunction : Number -> Number -> Number\n\
      \topLevelFunction = \\x y -> x + y"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelFunction",
          Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelFunction
              ( Ipe.Grammar.IpeFunction
                  { Ipe.Grammar.arguments = ["x", "y"],
                    Ipe.Grammar.functionBody =
                      Ipe.Grammar.IpeFunctionBody
                        { Ipe.Grammar.attributions = [],
                          Ipe.Grammar.functionReturn =
                            Ipe.Grammar.IpeBinaryOperation
                              Ipe.Grammar.Add
                              (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                              (Ipe.Grammar.IpeFunctionCallOrValue "y" [])
                        }
                  }
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation =
            Just
              ( Ipe.Grammar.TypeAnnotation
                  { Ipe.Grammar.typeAnnotationName = "topLevelFunction",
                    Ipe.Grammar.typeAnnotationArguments =
                      [ Ipe.Grammar.ConcreteType "Number" [],
                        Ipe.Grammar.ConcreteType "Number" []
                      ],
                    Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType "Number" []
                  }
              )
        }

  it "should parse a top level expression with type annotation" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelExpression : Number\n\
      \topLevelExpression = x + 1"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelExpression",
          Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelExpression
              ( Ipe.Grammar.IpeBinaryOperation
                  Ipe.Grammar.Add
                  (Ipe.Grammar.IpeFunctionCallOrValue "x" [])
                  (Ipe.Grammar.IpeNumber 1)
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation =
            Just
              ( Ipe.Grammar.TypeAnnotation
                  { Ipe.Grammar.typeAnnotationName = "topLevelExpression",
                    Ipe.Grammar.typeAnnotationArguments = [],
                    Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType "Number" []
                  }
              )
        }

  it "should parse a function with a single argument and a single return and a doc comment and type annotation" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "/*| A top level function that adds 1 to its argument */\n\
      \topLevelFunction : Number -> Number\n\
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
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation =
            Just
              ( Ipe.Grammar.TypeAnnotation
                  { Ipe.Grammar.typeAnnotationName = "topLevelFunction",
                    Ipe.Grammar.typeAnnotationArguments = [Ipe.Grammar.ConcreteType "Number" []],
                    Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType "Number" []
                  }
              )
        }

  it "should parse a top level expression with doc comment and type annotation" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "/*| A top level expression that equals x + 1 */\n\
      \topLevelExpression : Number\n\
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
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation =
            Just
              ( Ipe.Grammar.TypeAnnotation
                  { Ipe.Grammar.typeAnnotationName = "topLevelExpression",
                    Ipe.Grammar.typeAnnotationArguments = [],
                    Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ConcreteType "Number" []
                  }
              )
        }

  it "should parse a function with two record arguments and a single record return and type annotation" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelFunction : { a : Number } -> { b : Number } -> { a : Number }\n\
      \topLevelFunction = \\x y -> x"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelFunction",
          Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelFunction
              ( Ipe.Grammar.IpeFunction
                  { Ipe.Grammar.arguments = ["x", "y"],
                    Ipe.Grammar.functionBody =
                      Ipe.Grammar.IpeFunctionBody
                        { Ipe.Grammar.attributions = [],
                          Ipe.Grammar.functionReturn = Ipe.Grammar.IpeFunctionCallOrValue "x" []
                        }
                  }
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation =
            Just
              ( Ipe.Grammar.TypeAnnotation
                  { Ipe.Grammar.typeAnnotationName = "topLevelFunction",
                    Ipe.Grammar.typeAnnotationArguments =
                      [ Ipe.Grammar.RecordType $ Map.fromList [("a", Ipe.Grammar.ConcreteType "Number" [])],
                        Ipe.Grammar.RecordType $ Map.fromList [("b", Ipe.Grammar.ConcreteType "Number" [])]
                      ],
                    Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.RecordType $ Map.fromList [("a", Ipe.Grammar.ConcreteType "Number" [])]
                  }
              )
        }

  it "should parse a function with two generic arguments and a single generic return and type annotation" $ do
    Parsec.Common.parse
      Ipe.Parser.TopLevelDefinition.parser
      ""
      "topLevelFunction : a -> b -> b\n\
      \topLevelFunction = \\x y -> y"
      `shouldParse` Ipe.Grammar.TopLevelDefinition
        { Ipe.Grammar.topLevelDefinitionName = "topLevelFunction",
          Ipe.Grammar.topLevelDefinitionDocComment = Nothing,
          Ipe.Grammar.topLevelDefinitionValue =
            Ipe.Grammar.TopLevelFunction
              ( Ipe.Grammar.IpeFunction
                  { Ipe.Grammar.arguments = ["x", "y"],
                    Ipe.Grammar.functionBody =
                      Ipe.Grammar.IpeFunctionBody
                        { Ipe.Grammar.attributions = [],
                          Ipe.Grammar.functionReturn = Ipe.Grammar.IpeFunctionCallOrValue "y" []
                        }
                  }
              ),
          Ipe.Grammar.topLevelDefinitionTypeAnnotation =
            Just
              ( Ipe.Grammar.TypeAnnotation
                  { Ipe.Grammar.typeAnnotationName = "topLevelFunction",
                    Ipe.Grammar.typeAnnotationArguments =
                      [ Ipe.Grammar.ParameterType "a",
                        Ipe.Grammar.ParameterType "b"
                      ],
                    Ipe.Grammar.typeAnnotationReturnType = Ipe.Grammar.ParameterType "b"
                  }
              )
        }
