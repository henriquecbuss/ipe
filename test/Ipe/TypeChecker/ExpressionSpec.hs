{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.ExpressionSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Ipe.Grammar
import qualified Ipe.TypeChecker.Expression as ExprTypeChecker
import Ipe.TypeChecker.Utils (Error (..))
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "the expression type checker" $ do
    numberAndStringSpec
    binaryOperationSpec
    functionCallOrValueSpec
    functionDefinitionSpec
    matchSpec

simpleVariable :: T.Text -> Ipe.Grammar.Expression
simpleVariable varName =
  simpleFunction varName []

simpleFunction :: T.Text -> [Ipe.Grammar.Expression] -> Ipe.Grammar.Expression
simpleFunction fnName args =
  IpeFunctionCallOrValue $ FunctionCallOrValue [] fnName [] args

simpleRecordAccessor :: T.Text -> [T.Text] -> [Ipe.Grammar.Expression] -> Ipe.Grammar.Expression
simpleRecordAccessor fnName recordAccessors args =
  IpeFunctionCallOrValue $ FunctionCallOrValue [] fnName recordAccessors args

numberAndStringSpec :: Spec
numberAndStringSpec =
  describe "when dealing with numbers and strings" $ do
    it "should type check a basic number" $
      ExprTypeChecker.run (IpeNumber 1)
        `shouldBe` Right ExprTypeChecker.TNum

    prop "should type check any float" $
      \x ->
        ExprTypeChecker.run (IpeNumber x)
          `shouldBe` Right ExprTypeChecker.TNum

    it "should type check a basic string" $
      ExprTypeChecker.run (IpeString "hello")
        `shouldBe` Right ExprTypeChecker.TStr

    prop "should type check any string" $
      \x ->
        ExprTypeChecker.run (IpeString $ T.pack x)
          `shouldBe` Right ExprTypeChecker.TStr

binaryOperationSpec :: Spec
binaryOperationSpec =
  describe "when dealing with binary operations" $ do
    prop "should type check adding two numbers" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Add (IpeNumber x) (IpeNumber y))
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should not type check adding a number and something else" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Add (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should type check subtracting two numbers" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Subtract (IpeNumber x) (IpeNumber y))
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should not type check subtracting a number and something else" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Subtract (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should type check dividing two numbers" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Divide (IpeNumber x) (IpeNumber y))
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should not type check dividing a number and something else" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Divide (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should type check multiplying two numbers" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Multiply (IpeNumber x) (IpeNumber y))
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should not type check multiplying a number and something else" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Multiply (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should type check exponentiation with two numbers" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeNumber y))
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should type check exponentiation with a number and something else" $
      \x y ->
        ExprTypeChecker.run (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should type check basic pipe right" $
      \x ->
        let initialState = Map.singleton "pipeRightFn" (ExprTypeChecker.TFun ExprTypeChecker.TNum ExprTypeChecker.TNum)
         in ExprTypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeRight
                  (IpeNumber x)
                  (simpleVariable "pipeRightFn")
              )
              `shouldBe` Right ExprTypeChecker.TNum

    prop "should type check basic pipe left" $
      \x ->
        let initialState = Map.singleton "pipeLeftFn" (ExprTypeChecker.TFun ExprTypeChecker.TNum ExprTypeChecker.TNum)
         in ExprTypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeLeft
                  (simpleVariable "pipeLeftFn")
                  (IpeNumber x)
              )
              `shouldBe` Right ExprTypeChecker.TNum

    prop "should not type check basic pipe right with invalid argument type" $
      \x ->
        let initialState = Map.singleton "pipeRightFn" (ExprTypeChecker.TFun ExprTypeChecker.TStr ExprTypeChecker.TNum)
         in ExprTypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeRight
                  (IpeNumber x)
                  (simpleVariable "pipeRightFn")
              )
              `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should not type check basic pipe left with invalid argument type" $
      \x ->
        let initialState = Map.singleton "pipeLeftFn" (ExprTypeChecker.TFun ExprTypeChecker.TStr ExprTypeChecker.TNum)
         in ExprTypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeLeft
                  (simpleVariable "pipeLeftFn")
                  (IpeNumber x)
              )
              `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

functionCallOrValueSpec :: Spec
functionCallOrValueSpec =
  describe "when dealing with function calls or values" $ do
    it "should type check a simple value" $
      ExprTypeChecker.runWith (Map.fromList [("x", ExprTypeChecker.TNum)]) (simpleVariable "x")
        `shouldBe` Right ExprTypeChecker.TNum

    prop "should type check a simple function call with one argument" $
      \x ->
        ExprTypeChecker.runWith (Map.fromList [("x", ExprTypeChecker.TFun ExprTypeChecker.TNum ExprTypeChecker.TNum)]) (simpleFunction "x" [Ipe.Grammar.IpeNumber x])
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should not type check a simple function call with one mis-typed argument" $
      \x ->
        ExprTypeChecker.runWith (Map.fromList [("x", ExprTypeChecker.TFun ExprTypeChecker.TNum ExprTypeChecker.TNum)]) (simpleFunction "x" [Ipe.Grammar.IpeString $ T.pack x])
          `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should type check a function call with a few arguments" $
      \x y ->
        ExprTypeChecker.runWith
          ( Map.fromList
              [ ("a", ExprTypeChecker.TFun ExprTypeChecker.TNum (ExprTypeChecker.TFun ExprTypeChecker.TNum (ExprTypeChecker.TFun ExprTypeChecker.TNum (ExprTypeChecker.TFun ExprTypeChecker.TNum (ExprTypeChecker.TFun ExprTypeChecker.TStr ExprTypeChecker.TNum))))),
                ("b", ExprTypeChecker.TNum),
                ("c", ExprTypeChecker.TFun ExprTypeChecker.TNum ExprTypeChecker.TNum),
                ("d", ExprTypeChecker.TNum),
                ("e", ExprTypeChecker.TNum)
              ]
          )
          ( simpleFunction
              "a"
              [ simpleVariable "b",
                simpleFunction "c" [simpleVariable "d"],
                simpleVariable "e",
                IpeNumber x,
                IpeString $ T.pack y
              ]
          )
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should type check the structure of a simple flat record" $
      \x y ->
        ExprTypeChecker.run (IpeRecord [("x", IpeNumber x), ("y", IpeString $ T.pack y)])
          `shouldBe` Right (ExprTypeChecker.TRec [("x", ExprTypeChecker.TNum), ("y", ExprTypeChecker.TStr)])

    prop "should type check the structure of a complex nested record" $
      \a e f h l ->
        ExprTypeChecker.runWith
          (Map.singleton "d" (ExprTypeChecker.TVar "dVar"))
          ( IpeRecord
              [ ("a", IpeNumber a),
                ( "b",
                  IpeRecord
                    [ ( "c",
                        IpeRecord
                          [ ("d", simpleVariable "d"),
                            ("e", IpeNumber e)
                          ]
                      ),
                      ("f", IpeString $ T.pack f),
                      ("g", IpeRecord [])
                    ]
                ),
                ("h", IpeString $ T.pack h),
                ("k", IpeRecord [("l", IpeNumber l)])
              ]
          )
          `shouldBe` Right
            ( ExprTypeChecker.TRec
                [ ("a", ExprTypeChecker.TNum),
                  ( "b",
                    ExprTypeChecker.TRec
                      [ ( "c",
                          ExprTypeChecker.TRec
                            [ ("d", ExprTypeChecker.TVar "dVar"),
                              ("e", ExprTypeChecker.TNum)
                            ]
                        ),
                        ("f", ExprTypeChecker.TStr),
                        ("g", ExprTypeChecker.TRec [])
                      ]
                  ),
                  ("h", ExprTypeChecker.TStr),
                  ( "k",
                    ExprTypeChecker.TRec
                      [ ("l", ExprTypeChecker.TNum)
                      ]
                  )
                ]
            )

functionDefinitionSpec :: Spec
functionDefinitionSpec =
  describe "when dealing with function definitions" $ do
    it "should type check the identity function" $
      ExprTypeChecker.run (IpeFunction ["x"] (IpeFunctionBody [] (simpleVariable "x")))
        `shouldBe` Right (ExprTypeChecker.TFun (ExprTypeChecker.TVar "a0") (ExprTypeChecker.TVar "a0"))

    prop "should type check a function that returns a number" $
      \x ->
        ExprTypeChecker.run (IpeFunction ["x"] (IpeFunctionBody [] (IpeNumber x)))
          `shouldBe` Right (ExprTypeChecker.TFun (ExprTypeChecker.TVar "a0") ExprTypeChecker.TNum)

    prop "should type check a function that returns an addition" $
      \x y ->
        ExprTypeChecker.run (IpeFunction ["x", "y"] (IpeFunctionBody [] (IpeBinaryOperation Add (IpeNumber x) (IpeNumber y))))
          `shouldBe` Right (ExprTypeChecker.TFun (ExprTypeChecker.TVar "a0") (ExprTypeChecker.TFun (ExprTypeChecker.TVar "a1") ExprTypeChecker.TNum))

    prop "should type check a function that has a body" $
      \x ->
        ExprTypeChecker.run
          ( IpeFunction
              ["x"]
              ( IpeFunctionBody
                  [ ("y", IpeNumber 5)
                  ]
                  (IpeBinaryOperation Add (IpeNumber x) (simpleVariable "y"))
              )
          )
          `shouldBe` Right (ExprTypeChecker.TFun (ExprTypeChecker.TVar "a0") ExprTypeChecker.TNum)

    prop "should not type check adding a number and a variable that is a String" $
      \x y ->
        ExprTypeChecker.run
          ( IpeFunction
              ["x"]
              ( IpeFunctionBody
                  [ ("y", IpeString (T.pack y))
                  ]
                  (IpeBinaryOperation Add (IpeNumber x) (simpleVariable "y"))
              )
          )
          `shouldBe` Left (NoMatch ExprTypeChecker.TNum ExprTypeChecker.TStr)

    prop "should type check a function that has a few attributions in its body" $
      \a b c d e ->
        ExprTypeChecker.run
          ( IpeFunction
              ["x"]
              ( IpeFunctionBody
                  [ ("a", IpeNumber a),
                    ("b", IpeString $ T.pack b),
                    ("c", IpeNumber c),
                    ("d", IpeNumber d),
                    ("e", IpeString $ T.pack e)
                  ]
                  (IpeBinaryOperation Add (simpleVariable "a") (simpleVariable "c"))
              )
          )
          `shouldBe` Right (ExprTypeChecker.TFun (ExprTypeChecker.TVar "a0") ExprTypeChecker.TNum)

    prop "should type check calling a function from inside a record" $
      \x y ->
        let initialState = Map.singleton "x" (ExprTypeChecker.TRec [("a", ExprTypeChecker.TFun ExprTypeChecker.TNum (ExprTypeChecker.TFun ExprTypeChecker.TStr ExprTypeChecker.TNum))])
         in ExprTypeChecker.runWith initialState (simpleRecordAccessor "x" ["a"] [IpeNumber x, IpeString $ T.pack y])
              `shouldBe` Right ExprTypeChecker.TNum

    it "should type check calling a value from inside a nested record" $
      let initialState =
            Map.singleton
              "x"
              ( ExprTypeChecker.TRec
                  [ ( "a",
                      ExprTypeChecker.TRec
                        [ ("b", ExprTypeChecker.TNum),
                          ( "c",
                            ExprTypeChecker.TRec
                              [ ("d", ExprTypeChecker.TNum)
                              ]
                          )
                        ]
                    ),
                    ("e", ExprTypeChecker.TNum)
                  ]
              )
       in ExprTypeChecker.runWith initialState (simpleRecordAccessor "x" ["a", "c", "d"] [])
            `shouldBe` Right ExprTypeChecker.TNum

    prop "should type check a function based on its body" $
      \x y ->
        ExprTypeChecker.run
          ( IpeFunction
              ["a"]
              ( IpeFunctionBody
                  [ ("y", IpeNumber y),
                    ("x", IpeNumber x)
                  ]
                  ( IpeBinaryOperation
                      Add
                      (simpleVariable "a")
                      (IpeBinaryOperation Add (simpleVariable "x") (simpleVariable "y"))
                  )
              )
          )
          `shouldBe` Right (ExprTypeChecker.TFun ExprTypeChecker.TNum ExprTypeChecker.TNum)

matchSpec :: Spec
matchSpec =
  describe "when dealing with pattern matching" $ do
    describe "when pattern matching on numbers" $ do
      prop "should type check a simple case expression" $
        \x ->
          ExprTypeChecker.run
            ( IpeMatch
                (IpeNumber x)
                [ (IpeWildCardPattern, [], IpeNumber x)
                ]
            )
            `shouldBe` Right ExprTypeChecker.TNum

      prop "should check for exhaustiveness" $
        \x ->
          ExprTypeChecker.run
            ( IpeMatch
                (IpeNumber x)
                [ (IpeLiteralNumberPattern x, [], IpeNumber x)
                ]
            )
            `shouldBe` Left MissingPatternMatchCases

      prop "should not allow duplicate matching" $
        \x ->
          ExprTypeChecker.run
            ( IpeMatch
                (IpeNumber x)
                [ (IpeLiteralNumberPattern x, [], IpeNumber x),
                  (IpeLiteralNumberPattern x, [], IpeNumber x)
                ]
            )
            `shouldBe` Left DuplicatePatternMatch

    describe "when pattern matching on strings" $ do
      prop "should type check a simple case expression" $
        \x ->
          ExprTypeChecker.run
            ( IpeMatch
                (IpeString $ T.pack x)
                [ (IpeWildCardPattern, [], IpeString $ T.pack x)
                ]
            )
            `shouldBe` Right ExprTypeChecker.TStr

      prop "should check for exhaustiveness" $
        \x ->
          ExprTypeChecker.run
            ( IpeMatch
                (IpeString $ T.pack x)
                [ (IpeLiteralStringPattern $ T.pack x, [], IpeString $ T.pack x)
                ]
            )
            `shouldBe` Left MissingPatternMatchCases

      prop "should not allow duplicate matching" $
        \x ->
          ExprTypeChecker.run
            ( IpeMatch
                (IpeString $ T.pack x)
                [ (IpeLiteralStringPattern $ T.pack x, [], IpeString $ T.pack x),
                  (IpeLiteralStringPattern $ T.pack x, [], IpeString $ T.pack x)
                ]
            )
            `shouldBe` Left DuplicatePatternMatch

    prop "should infer based on branch attributions" $
      \x y z ->
        ExprTypeChecker.run
          ( IpeMatch
              (IpeString $ T.pack x)
              [ ( IpeWildCardPattern,
                  [ ("y", IpeNumber y),
                    ("z", IpeNumber z),
                    ("returnType", IpeBinaryOperation Add (simpleVariable "y") (simpleVariable "z"))
                  ],
                  simpleVariable "returnType"
                )
              ]
          )
          `shouldBe` Right ExprTypeChecker.TNum

    prop "should check that all branches are the same type" $
      \b c d ->
        ExprTypeChecker.runWith
          (Map.singleton "x" (ExprTypeChecker.TVar "a0"))
          ( IpeMatch
              (simpleVariable "x")
              [ (IpeLiteralStringPattern $ T.pack b, [], Ipe.Grammar.IpeNumber d),
                (IpeLiteralNumberPattern c, [], Ipe.Grammar.IpeNumber d)
              ]
          )
          `shouldBe` Left InvalidTypeForPatternMatch

    prop "should infer type based on patterns" $
      \b ->
        ExprTypeChecker.runWith
          (Map.singleton "x" (ExprTypeChecker.TVar "a0"))
          ( IpeMatch
              (simpleVariable "x")
              [ (IpeLiteralStringPattern $ T.pack b, [], simpleVariable "x"),
                (IpeWildCardPattern, [], simpleVariable "x")
              ]
          )
          `shouldBe` Right ExprTypeChecker.TStr

    prop "should type check when using custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    ExprTypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                      ]
                  ),
                  ("x", ExprTypeChecker.TVar "a0")
                ]
         in ExprTypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a),
                    (IpeCustomTypePattern ["Module"] "ConstructorTwo" ["a", "b"], [], simpleVariable "a")
                  ]
              )
              `shouldBe` Right ExprTypeChecker.TNum

    prop "should check for exhaustiveness when using custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    ExprTypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                      ]
                  ),
                  ("x", ExprTypeChecker.TVar "a0")
                ]
         in ExprTypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a)
                  ]
              )
              `shouldBe` Left MissingPatternMatchCases

    prop "should type check pattern matching on custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    ExprTypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                      ]
                  ),
                  ( "x",
                    ExprTypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                      ]
                  )
                ]
         in ExprTypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a),
                    (IpeCustomTypePattern ["Module"] "ConstructorTwo" ["a", "b"], [], simpleVariable "a")
                  ]
              )
              `shouldBe` Right ExprTypeChecker.TNum

    prop "should check for exhaustiveness pattern matching on custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    ExprTypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                      ]
                  ),
                  ( "x",
                    ExprTypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                      ]
                  )
                ]
         in ExprTypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a)
                  ]
              )
              `shouldBe` Left MissingPatternMatchCases

    it "should infer type arguments when pattern matching on custom types" $
      let initialState =
            Map.fromList
              [ ( "Module.CustomType",
                  ExprTypeChecker.TCustom
                    "Module.CustomType"
                    [ExprTypeChecker.TVar "someVar"]
                    [ ("Module.ConstructorOne", [ExprTypeChecker.TVar "someVar"]),
                      ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                    ]
                ),
                ( "x",
                  ExprTypeChecker.TCustom
                    "Module.CustomType"
                    [ExprTypeChecker.TVar "someVar"]
                    [ ("Module.ConstructorOne", [ExprTypeChecker.TVar "someVar"]),
                      ("Module.ConstructorTwo", [ExprTypeChecker.TNum, ExprTypeChecker.TStr])
                    ]
                )
              ]
       in ExprTypeChecker.runWith
            initialState
            ( IpeMatch
                (simpleVariable "x")
                [ (IpeCustomTypePattern ["Module"] "ConstructorOne" ["a"], [], simpleVariable "a"),
                  (IpeCustomTypePattern ["Module"] "ConstructorTwo" ["a", "b"], [], simpleVariable "a")
                ]
            )
            `shouldBe` Right ExprTypeChecker.TNum
