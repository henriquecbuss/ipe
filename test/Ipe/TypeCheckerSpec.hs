{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeCheckerSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Ipe.Grammar
import qualified Ipe.TypeChecker as TypeChecker
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "the expression type checker2" $ do
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
      TypeChecker.run (IpeNumber 1)
        `shouldBe` Right TypeChecker.TNum

    prop "should type check any float" $
      \x ->
        TypeChecker.run (IpeNumber x)
          `shouldBe` Right TypeChecker.TNum

    it "should type check a basic string" $
      TypeChecker.run (IpeString "hello")
        `shouldBe` Right TypeChecker.TStr

    prop "should type check any string" $
      \x ->
        TypeChecker.run (IpeString $ T.pack x)
          `shouldBe` Right TypeChecker.TStr

binaryOperationSpec :: Spec
binaryOperationSpec =
  describe "when dealing with binary operations" $ do
    prop "should type check adding two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Add (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check adding a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Add (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should type check subtracting two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Subtract (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check subtracting a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Subtract (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should type check dividing two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Divide (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check dividing a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Divide (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should type check multiplying two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Multiply (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check multiplying a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Multiply (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should type check exponentiation with two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should type check exponentiation with a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should type check basic pipe right" $
      \x ->
        let initialState = Map.singleton "pipeRightFn" (TypeChecker.TFun TypeChecker.TNum TypeChecker.TNum)
         in TypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeRight
                  (IpeNumber x)
                  (simpleVariable "pipeRightFn")
              )
              `shouldBe` Right TypeChecker.TNum

    prop "should type check basic pipe left" $
      \x ->
        let initialState = Map.singleton "pipeLeftFn" (TypeChecker.TFun TypeChecker.TNum TypeChecker.TNum)
         in TypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeLeft
                  (simpleVariable "pipeLeftFn")
                  (IpeNumber x)
              )
              `shouldBe` Right TypeChecker.TNum

    prop "should not type check basic pipe right with invalid argument type" $
      \x ->
        let initialState = Map.singleton "pipeRightFn" (TypeChecker.TFun TypeChecker.TStr TypeChecker.TNum)
         in TypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeRight
                  (IpeNumber x)
                  (simpleVariable "pipeRightFn")
              )
              `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should not type check basic pipe left with invalid argument type" $
      \x ->
        let initialState = Map.singleton "pipeLeftFn" (TypeChecker.TFun TypeChecker.TStr TypeChecker.TNum)
         in TypeChecker.runWith
              initialState
              ( IpeBinaryOperation
                  PipeLeft
                  (simpleVariable "pipeLeftFn")
                  (IpeNumber x)
              )
              `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

functionCallOrValueSpec :: Spec
functionCallOrValueSpec =
  describe "when dealing with function calls or values" $ do
    it "should type check a simple value" $
      TypeChecker.runWith (Map.fromList [("x", TypeChecker.TNum)]) (simpleVariable "x")
        `shouldBe` Right TypeChecker.TNum

    prop "should type check a simple function call with one argument" $
      \x ->
        TypeChecker.runWith (Map.fromList [("x", TypeChecker.TFun TypeChecker.TNum TypeChecker.TNum)]) (simpleFunction "x" [Ipe.Grammar.IpeNumber x])
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check a simple function call with one mis-typed argument" $
      \x ->
        TypeChecker.runWith (Map.fromList [("x", TypeChecker.TFun TypeChecker.TNum TypeChecker.TNum)]) (simpleFunction "x" [Ipe.Grammar.IpeString $ T.pack x])
          `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should type check a function call with a few arguments" $
      \x y ->
        TypeChecker.runWith
          ( Map.fromList
              [ ("a", TypeChecker.TFun TypeChecker.TNum (TypeChecker.TFun TypeChecker.TNum (TypeChecker.TFun TypeChecker.TNum (TypeChecker.TFun TypeChecker.TNum (TypeChecker.TFun TypeChecker.TStr TypeChecker.TNum))))),
                ("b", TypeChecker.TNum),
                ("c", TypeChecker.TFun TypeChecker.TNum TypeChecker.TNum),
                ("d", TypeChecker.TNum),
                ("e", TypeChecker.TNum)
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
          `shouldBe` Right TypeChecker.TNum

    prop "should type check the structure of a simple flat record" $
      \x y ->
        TypeChecker.run (IpeRecord [("x", IpeNumber x), ("y", IpeString $ T.pack y)])
          `shouldBe` Right (TypeChecker.TRec [("x", TypeChecker.TNum), ("y", TypeChecker.TStr)])

    prop "should type check the structure of a complex nested record" $
      \a e f h l ->
        TypeChecker.runWith
          (Map.singleton "d" (TypeChecker.TVar "dVar"))
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
            ( TypeChecker.TRec
                [ ("a", TypeChecker.TNum),
                  ( "b",
                    TypeChecker.TRec
                      [ ( "c",
                          TypeChecker.TRec
                            [ ("d", TypeChecker.TVar "dVar"),
                              ("e", TypeChecker.TNum)
                            ]
                        ),
                        ("f", TypeChecker.TStr),
                        ("g", TypeChecker.TRec [])
                      ]
                  ),
                  ("h", TypeChecker.TStr),
                  ( "k",
                    TypeChecker.TRec
                      [ ("l", TypeChecker.TNum)
                      ]
                  )
                ]
            )

functionDefinitionSpec :: Spec
functionDefinitionSpec =
  describe "when dealing with function definitions" $ do
    it "should type check the identity function" $
      TypeChecker.run (IpeFunction ["x"] (IpeFunctionBody [] (simpleVariable "x")))
        `shouldBe` Right (TypeChecker.TFun (TypeChecker.TVar "a0") (TypeChecker.TVar "a0"))

    prop "should type check a function that returns a number" $
      \x ->
        TypeChecker.run (IpeFunction ["x"] (IpeFunctionBody [] (IpeNumber x)))
          `shouldBe` Right (TypeChecker.TFun (TypeChecker.TVar "a0") TypeChecker.TNum)

    prop "should type check a function that returns an addition" $
      \x y ->
        TypeChecker.run (IpeFunction ["x", "y"] (IpeFunctionBody [] (IpeBinaryOperation Add (IpeNumber x) (IpeNumber y))))
          `shouldBe` Right (TypeChecker.TFun (TypeChecker.TVar "a0") (TypeChecker.TFun (TypeChecker.TVar "a1") TypeChecker.TNum))

    prop "should type check a function that has a body" $
      \x ->
        TypeChecker.run
          ( IpeFunction
              ["x"]
              ( IpeFunctionBody
                  [ ("y", IpeNumber 5)
                  ]
                  (IpeBinaryOperation Add (IpeNumber x) (simpleVariable "y"))
              )
          )
          `shouldBe` Right (TypeChecker.TFun (TypeChecker.TVar "a0") TypeChecker.TNum)

    prop "should not type check adding a number and a variable that is a String" $
      \x y ->
        TypeChecker.run
          ( IpeFunction
              ["x"]
              ( IpeFunctionBody
                  [ ("y", IpeString (T.pack y))
                  ]
                  (IpeBinaryOperation Add (IpeNumber x) (simpleVariable "y"))
              )
          )
          `shouldBe` Left "can't match expected type\n\tNumber\nwith actual type\n\tString"

    prop "should type check a function that has a few attributions in its body" $
      \a b c d e ->
        TypeChecker.run
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
          `shouldBe` Right (TypeChecker.TFun (TypeChecker.TVar "a0") TypeChecker.TNum)

    prop "should type check calling a function from inside a record" $
      \x y ->
        let initialState = Map.singleton "x" (TypeChecker.TRec [("a", TypeChecker.TFun TypeChecker.TNum (TypeChecker.TFun TypeChecker.TStr TypeChecker.TNum))])
         in TypeChecker.runWith initialState (simpleRecordAccessor "x" ["a"] [IpeNumber x, IpeString $ T.pack y])
              `shouldBe` Right TypeChecker.TNum

    it "should type check calling a value from inside a nested record" $
      let initialState =
            Map.singleton
              "x"
              ( TypeChecker.TRec
                  [ ( "a",
                      TypeChecker.TRec
                        [ ("b", TypeChecker.TNum),
                          ( "c",
                            TypeChecker.TRec
                              [ ("d", TypeChecker.TNum)
                              ]
                          )
                        ]
                    ),
                    ("e", TypeChecker.TNum)
                  ]
              )
       in TypeChecker.runWith initialState (simpleRecordAccessor "x" ["a", "c", "d"] [])
            `shouldBe` Right TypeChecker.TNum

    prop "should type check a function based on its body" $
      \x y ->
        TypeChecker.run
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
          `shouldBe` Right (TypeChecker.TFun TypeChecker.TNum TypeChecker.TNum)

matchSpec :: Spec
matchSpec =
  describe "when dealing with pattern matching" $ do
    describe "when pattern matching on numbers" $ do
      prop "should type check a simple case expression" $
        \x ->
          TypeChecker.run
            ( IpeMatch
                (IpeNumber x)
                [ (IpeWildCardPattern, [], IpeNumber x)
                ]
            )
            `shouldBe` Right TypeChecker.TNum

      prop "should check for exhaustiveness" $
        \x ->
          TypeChecker.run
            ( IpeMatch
                (IpeNumber x)
                [ (IpeLiteralNumberPattern x, [], IpeNumber x)
                ]
            )
            `shouldBe` Left "can't pattern match on a number with a finite pattern match without matching all possible cases."

      prop "should not allow duplicate matching" $
        \x ->
          TypeChecker.run
            ( IpeMatch
                (IpeNumber x)
                [ (IpeLiteralNumberPattern x, [], IpeNumber x),
                  (IpeLiteralNumberPattern x, [], IpeNumber x)
                ]
            )
            `shouldBe` Left ("number " ++ show x ++ " is already pattern matched.")

    describe "when pattern matching on strings" $ do
      prop "should type check a simple case expression" $
        \x ->
          TypeChecker.run
            ( IpeMatch
                (IpeString $ T.pack x)
                [ (IpeWildCardPattern, [], IpeString $ T.pack x)
                ]
            )
            `shouldBe` Right TypeChecker.TStr

      prop "should check for exhaustiveness" $
        \x ->
          TypeChecker.run
            ( IpeMatch
                (IpeString $ T.pack x)
                [ (IpeLiteralStringPattern $ T.pack x, [], IpeString $ T.pack x)
                ]
            )
            `shouldBe` Left "can't pattern match on a string with a finite pattern match without matching all possible cases."

      prop "should not allow duplicate matching" $
        \x ->
          TypeChecker.run
            ( IpeMatch
                (IpeString $ T.pack x)
                [ (IpeLiteralStringPattern $ T.pack x, [], IpeString $ T.pack x),
                  (IpeLiteralStringPattern $ T.pack x, [], IpeString $ T.pack x)
                ]
            )
            `shouldBe` Left ("string " ++ show x ++ " is already pattern matched.")

    prop "should infer based on branch attributions" $
      \x y z ->
        TypeChecker.run
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
          `shouldBe` Right TypeChecker.TNum

    prop "should check that all branches are the same type" $
      \b c d ->
        TypeChecker.runWith
          (Map.singleton "x" (TypeChecker.TVar "a0"))
          ( IpeMatch
              (simpleVariable "x")
              [ (IpeLiteralStringPattern $ T.pack b, [], Ipe.Grammar.IpeNumber d),
                (IpeLiteralNumberPattern c, [], Ipe.Grammar.IpeNumber d)
              ]
          )
          `shouldBe` Left "Number type does not match the type from previous branches, which was String."

    prop "should infer type based on patterns" $
      \b ->
        TypeChecker.runWith
          (Map.singleton "x" (TypeChecker.TVar "a0"))
          ( IpeMatch
              (simpleVariable "x")
              [ (IpeLiteralStringPattern $ T.pack b, [], simpleVariable "x"),
                (IpeWildCardPattern, [], simpleVariable "x")
              ]
          )
          `shouldBe` Right TypeChecker.TStr

    prop "should type check when using custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    TypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                      ]
                  ),
                  ("x", TypeChecker.TVar "a0")
                ]
         in TypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a),
                    (IpeCustomTypePattern ["Module"] "ConstructorTwo" ["a", "b"], [], simpleVariable "a")
                  ]
              )
              `shouldBe` Right TypeChecker.TNum

    prop "should check for exhaustiveness when using custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    TypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                      ]
                  ),
                  ("x", TypeChecker.TVar "a0")
                ]
         in TypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a)
                  ]
              )
              `shouldBe` Left "can't pattern match on a custom type (Module.CustomType) with a finite pattern match without matching all possible cases."

    prop "should type check pattern matching on custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    TypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                      ]
                  ),
                  ( "x",
                    TypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                      ]
                  )
                ]
         in TypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a),
                    (IpeCustomTypePattern ["Module"] "ConstructorTwo" ["a", "b"], [], simpleVariable "a")
                  ]
              )
              `shouldBe` Right TypeChecker.TNum

    prop "should check for exhaustiveness pattern matching on custom types" $
      \a ->
        let initialState =
              Map.fromList
                [ ( "Module.CustomType",
                    TypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                      ]
                  ),
                  ( "x",
                    TypeChecker.TCustom
                      "Module.CustomType"
                      []
                      [ ("Module.ConstructorOne", []),
                        ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                      ]
                  )
                ]
         in TypeChecker.runWith
              initialState
              ( IpeMatch
                  (simpleVariable "x")
                  [ (IpeCustomTypePattern ["Module"] "ConstructorOne" [], [], IpeNumber a)
                  ]
              )
              `shouldBe` Left "can't pattern match on a custom type (Module.CustomType) with a finite pattern match without matching all possible cases."

    it "should infer type arguments when pattern matching on custom types" $
      let initialState =
            Map.fromList
              [ ( "Module.CustomType",
                  TypeChecker.TCustom
                    "Module.CustomType"
                    [TypeChecker.TVar "someVar"]
                    [ ("Module.ConstructorOne", [TypeChecker.TVar "someVar"]),
                      ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                    ]
                ),
                ( "x",
                  TypeChecker.TCustom
                    "Module.CustomType"
                    [TypeChecker.TVar "someVar"]
                    [ ("Module.ConstructorOne", [TypeChecker.TVar "someVar"]),
                      ("Module.ConstructorTwo", [TypeChecker.TNum, TypeChecker.TStr])
                    ]
                )
              ]
       in TypeChecker.runWith
            initialState
            ( IpeMatch
                (simpleVariable "x")
                [ (IpeCustomTypePattern ["Module"] "ConstructorOne" ["a"], [], simpleVariable "a"),
                  (IpeCustomTypePattern ["Module"] "ConstructorTwo" ["a", "b"], [], simpleVariable "a")
                ]
            )
            `shouldBe` Right TypeChecker.TNum
