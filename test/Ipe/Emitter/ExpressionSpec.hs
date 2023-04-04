{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter.ExpressionSpec (spec) where

import Ipe.Emitter.Expression (emit)
import Ipe.Grammar
import Test.Hspec

spec :: Spec
spec = describe "the expression emitter" $ do
  binaryOperation
  number
  string
  ipeMatch
  ipeFunctionCallOrValue
  ipeFunction
  ipeRecord

binaryOperation :: Spec
binaryOperation = do
  describe "when emitting binary operations" $ do
    it "should emit addition" $
      show (emit (IpeBinaryOperation Add (IpeNumber 1) (IpeNumber 2)))
        `shouldBe` "1.0 + 2.0"

    it "should emit subtraction" $
      show (emit (IpeBinaryOperation Subtract (IpeNumber 1) (IpeNumber 2)))
        `shouldBe` "1.0 - 2.0"

    it "should emit division" $
      show (emit (IpeBinaryOperation Divide (IpeNumber 1) (IpeNumber 2)))
        `shouldBe` "1.0 / 2.0"

    it "should emit multiplication" $
      show (emit (IpeBinaryOperation Multiply (IpeNumber 1) (IpeNumber 2)))
        `shouldBe` "1.0 * 2.0"

    it "should emit exponentiation" $
      show (emit (IpeBinaryOperation Exponentiation (IpeNumber 1) (IpeNumber 2)))
        `shouldBe` "1.0 ** 2.0"

    it "should emit pipe right" $
      show
        ( emit
            ( IpeBinaryOperation
                PipeRight
                (IpeNumber 1)
                ( IpeFunctionCallOrValue
                    ( FunctionCallOrValue
                        { functionCallOrValuePath = [],
                          functionCallOrValueName = "foo",
                          functionCallOrValueRecordAccessors = [],
                          functionCallOrValueArguments = []
                        }
                    )
                )
            )
        )
        `shouldBe` "foo(1.0)"

    it "should emit pipe left" $
      show
        ( emit
            ( IpeBinaryOperation
                PipeLeft
                ( IpeFunctionCallOrValue
                    ( FunctionCallOrValue
                        { functionCallOrValuePath = [],
                          functionCallOrValueName = "foo",
                          functionCallOrValueRecordAccessors = [],
                          functionCallOrValueArguments = []
                        }
                    )
                )
                (IpeNumber 1)
            )
        )
        `shouldBe` "foo(1.0)"

number :: Spec
number = do
  describe "when emitting numbers" $ do
    it "should emit an integer" $
      show (emit (IpeNumber 1)) `shouldBe` "1.0"

    it "should emit a float" $
      show (emit (IpeNumber 1.5)) `shouldBe` "1.5"

string :: Spec
string = do
  describe "when emitting strings" $ do
    it "should emit a string" $
      show (emit (IpeString "foo")) `shouldBe` "'foo'"

    it "should escape single quotes" $
      show (emit (IpeString "foo'bar")) `shouldBe` "'foo\\'bar'"

ipeMatch :: Spec
ipeMatch = do
  describe "when emitting pattern matches" $ do
    it "should emit a pattern match with a wildcard pattern" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ (IpeWildCardPattern, [], IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  return foo\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a wildcard pattern with some attributions" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ ( IpeWildCardPattern,
                    [ ("a", IpeNumber 1),
                      ("b", IpeNumber 2),
                      ("c", IpeNumber 3)
                    ],
                    IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] [])
                  )
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  const a = 1.0;\n\
                   \  const b = 2.0;\n\
                   \  const c = 3.0;\n\
                   \  return foo\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a variable pattern" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ (IpeVariablePattern "variable", [], IpeFunctionCallOrValue (FunctionCallOrValue [] "variable" [] []))
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  const variable = __ipeConst;\n\
                   \  return variable\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a variable pattern with some attributions" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ ( IpeVariablePattern "variable",
                    [ ("a", IpeNumber 1),
                      ("b", IpeNumber 2),
                      ("c", IpeNumber 3)
                    ],
                    IpeFunctionCallOrValue (FunctionCallOrValue [] "variable" [] [])
                  )
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  const variable = __ipeConst;\n\
                   \  const a = 1.0;\n\
                   \  const b = 2.0;\n\
                   \  const c = 3.0;\n\
                   \  return variable\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a number pattern" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ (IpeLiteralNumberPattern 1, [], IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  if (__ipeConst === 1.0) {\n\
                   \    return foo\n\
                   \  }\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a number pattern with some attributions" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ ( IpeLiteralNumberPattern 1,
                    [ ("a", IpeNumber 1),
                      ("b", IpeNumber 2),
                      ("c", IpeNumber 3)
                    ],
                    IpeFunctionCallOrValue (FunctionCallOrValue [] "variable" [] [])
                  )
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  if (__ipeConst === 1.0) {\n\
                   \    const a = 1.0;\n\
                   \    const b = 2.0;\n\
                   \    const c = 3.0;\n\
                   \    return variable\n\
                   \  }\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a string pattern" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ (IpeLiteralStringPattern "a", [], IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  if (__ipeConst === 'a') {\n\
                   \    return foo\n\
                   \  }\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a string pattern with some attributions" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ ( IpeLiteralStringPattern "a",
                    [ ("a", IpeNumber 1),
                      ("b", IpeNumber 2),
                      ("c", IpeNumber 3)
                    ],
                    IpeFunctionCallOrValue (FunctionCallOrValue [] "variable" [] [])
                  )
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  if (__ipeConst === 'a') {\n\
                   \    const a = 1.0;\n\
                   \    const b = 2.0;\n\
                   \    const c = 3.0;\n\
                   \    return variable\n\
                   \  }\n\
                   \}\n\
                   \()"

    it "should emit multiple branches when pattern matching on a number" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ (IpeLiteralNumberPattern 1, [], IpeFunctionCallOrValue (FunctionCallOrValue [] "One" [] [])),
                  (IpeLiteralNumberPattern 2, [], IpeFunctionCallOrValue (FunctionCallOrValue [] "Two" [] [])),
                  (IpeLiteralNumberPattern 3, [], IpeFunctionCallOrValue (FunctionCallOrValue [] "Three" [] [])),
                  (IpeLiteralNumberPattern 4, [], IpeFunctionCallOrValue (FunctionCallOrValue [] "Four" [] [])),
                  (IpeWildCardPattern, [], IpeFunctionCallOrValue (FunctionCallOrValue [] "Other" [] []))
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  if (__ipeConst === 1.0) {\n\
                   \    return One\n\
                   \  }\n\
                   \\n\
                   \  if (__ipeConst === 2.0) {\n\
                   \    return Two\n\
                   \  }\n\
                   \\n\
                   \  if (__ipeConst === 3.0) {\n\
                   \    return Three\n\
                   \  }\n\
                   \\n\
                   \  if (__ipeConst === 4.0) {\n\
                   \    return Four\n\
                   \  }\n\
                   \\n\
                   \  return Other\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a custom type pattern" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ (IpeCustomTypePattern [] "One" [], [], IpeNumber 1)
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  if (__ipeConst[0] === 'One') {\n\
                   \    return 1.0\n\
                   \  }\n\
                   \}\n\
                   \()"

    it "should emit a pattern match with a custom type pattern with args" $
      show
        ( emit
            ( IpeMatch
                (IpeFunctionCallOrValue (FunctionCallOrValue [] "foo" [] []))
                [ (IpeCustomTypePattern [] "One" ["a", "b", "c"], [], IpeNumber 1)
                ]
            )
        )
        `shouldBe` "() => {\n\
                   \  const __ipeConst = foo;\n\
                   \  if (__ipeConst[0] === 'One') {\n\
                   \    const [_, a, b, c] = __ipeConst;\n\
                   \    return 1.0\n\
                   \  }\n\
                   \}\n\
                   \()"

ipeFunctionCallOrValue :: Spec
ipeFunctionCallOrValue = do
  describe "when emitting function calls or values" $ do
    it "should emit a function call with no arguments" $
      show
        ( emit
            ( IpeFunctionCallOrValue
                ( FunctionCallOrValue
                    { functionCallOrValuePath = [],
                      functionCallOrValueName = "foo",
                      functionCallOrValueRecordAccessors = [],
                      functionCallOrValueArguments = []
                    }
                )
            )
        )
        `shouldBe` "foo"

    it "should emit a function call with arguments" $
      show
        ( emit
            ( IpeFunctionCallOrValue
                ( FunctionCallOrValue
                    { functionCallOrValuePath = [],
                      functionCallOrValueName = "foo",
                      functionCallOrValueRecordAccessors = [],
                      functionCallOrValueArguments = [IpeNumber 1, IpeNumber 2]
                    }
                )
            )
        )
        `shouldBe` "foo(1.0)(2.0)"

    it "should emit a function call with a path" $
      show
        ( emit
            ( IpeFunctionCallOrValue
                ( FunctionCallOrValue
                    { functionCallOrValuePath = ["foo", "bar"],
                      functionCallOrValueName = "baz",
                      functionCallOrValueRecordAccessors = [],
                      functionCallOrValueArguments = []
                    }
                )
            )
        )
        `shouldBe` "foo.bar.baz"

    it "should emit a function call with a record accessor" $
      show
        ( emit
            ( IpeFunctionCallOrValue
                ( FunctionCallOrValue
                    { functionCallOrValuePath = [],
                      functionCallOrValueName = "foo",
                      functionCallOrValueRecordAccessors = ["bar"],
                      functionCallOrValueArguments = []
                    }
                )
            )
        )
        `shouldBe` "foo.bar"

    it "should emit a function call with a path and a record accessor" $
      show
        ( emit
            ( IpeFunctionCallOrValue
                ( FunctionCallOrValue
                    { functionCallOrValuePath = ["foo", "bar"],
                      functionCallOrValueName = "baz",
                      functionCallOrValueRecordAccessors = ["qux"],
                      functionCallOrValueArguments = []
                    }
                )
            )
        )
        `shouldBe` "foo.bar.baz.qux"

ipeFunction :: Spec
ipeFunction = do
  describe "when emitting functions" $ do
    it "should emit a function with no arguments" $
      show
        ( emit
            ( IpeFunction
                []
                ( IpeFunctionBody
                    { functionBodyAttributions = [],
                      functionReturn =
                        IpeFunctionCallOrValue
                          (FunctionCallOrValue [] "foo" [] [])
                    }
                )
            )
        )
        `shouldBe` "() => {return foo}"

    it "should emit the identity function" $
      show
        ( emit
            ( IpeFunction
                ["x"]
                ( IpeFunctionBody
                    { functionBodyAttributions = [],
                      functionReturn =
                        IpeFunctionCallOrValue
                          (FunctionCallOrValue [] "x" [] [])
                    }
                )
            )
        )
        `shouldBe` "x => {return x}"

    it "should emit a function with some attributions" $
      show
        ( emit
            ( IpeFunction
                ["x"]
                ( IpeFunctionBody
                    { functionBodyAttributions =
                        [ ("y", IpeNumber 1),
                          ("z", IpeNumber 2),
                          ("w", IpeNumber 3)
                        ],
                      functionReturn =
                        IpeFunctionCallOrValue
                          (FunctionCallOrValue [] "x" [] [])
                    }
                )
            )
        )
        `shouldBe` "x => {\n\
                   \    const y = 1.0;\n\
                   \    const z = 2.0;\n\
                   \    const w = 3.0;\n\
                   \    return x\n\
                   \  }"

    it "should emit a function with some attributions and multiple arguments" $
      show
        ( emit
            ( IpeFunction
                ["a", "b", "c", "d"]
                ( IpeFunctionBody
                    { functionBodyAttributions =
                        [ ("y", IpeNumber 1),
                          ("z", IpeNumber 2),
                          ("w", IpeNumber 3)
                        ],
                      functionReturn =
                        IpeFunctionCallOrValue
                          (FunctionCallOrValue [] "w" [] [])
                    }
                )
            )
        )
        `shouldBe` "a => b => c => d => {\n\
                   \    const y = 1.0;\n\
                   \    const z = 2.0;\n\
                   \    const w = 3.0;\n\
                   \    return w\n\
                   \  }"

ipeRecord :: Spec
ipeRecord = do
  describe "when emitting records" $ do
    it "should emit an empty record" $
      show (emit (IpeRecord [])) `shouldBe` "{}"

    it "should emit a record with a single field" $
      show (emit (IpeRecord [("foo", IpeNumber 1)])) `shouldBe` "{'foo': 1.0}"

    it "should emit a record with multiple fields" $
      show
        ( emit
            ( IpeRecord
                [ ("foo", IpeNumber 1),
                  ("bar", IpeNumber 2)
                ]
            )
        )
        `shouldBe` "{'foo': 1.0, 'bar': 2.0}"
