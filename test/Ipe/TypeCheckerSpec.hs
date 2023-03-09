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
          `shouldBe` Left "can't add Number and String. I can only add two Numbers."

    prop "should type check subtracting two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Subtract (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check subtracting a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Subtract (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left "can't subtract Number and String. I can only subtract two Numbers."

    prop "should type check dividing two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Divide (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check dividing a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Divide (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` Left "can't divide String and Number. I can only divide two Numbers."

    prop "should type check multiplying two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Multiply (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should not type check multiplying a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Multiply (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` Left "can't multiply String and Number. I can only multiply two Numbers."

    prop "should type check exponentiation with two numbers" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeNumber y))
          `shouldBe` Right TypeChecker.TNum

    prop "should type check exponentiation with a number and something else" $
      \x y ->
        TypeChecker.run (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` Left "can't potentiate Number and String. I can only potentiate two Numbers."

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
          `shouldBe` Left "can't add Number and String. I can only add two Numbers."

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

simpleVariable :: T.Text -> Ipe.Grammar.Expression
simpleVariable varName =
  simpleFunction varName []

simpleFunction :: T.Text -> [Ipe.Grammar.Expression] -> Ipe.Grammar.Expression
simpleFunction fnName args =
  IpeFunctionCallOrValue $ FunctionCallOrValue [] fnName [] args