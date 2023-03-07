{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.ExpressionSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Ipe.Grammar
import qualified Ipe.TypeChecker as TypeChecker
import qualified Ipe.TypeChecker.Expression
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "the expression type checker" $ do
    numberAndStringSpec
    binaryOperationSpec

numberAndStringSpec :: Spec
numberAndStringSpec =
  describe "when dealing with numbers and strings" $ do
    it "should type check a basic number" $
      TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeNumber 1)
        `shouldBe` (Right TypeChecker.IpeNumber, Map.empty)

    prop "should type check any float" $
      \x ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeNumber x)
          `shouldBe` (Right TypeChecker.IpeNumber, Map.empty)

    it "should type check a basic string" $
      TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeString "hello")
        `shouldBe` (Right TypeChecker.IpeString, Map.empty)

    prop "should type check any string" $
      \x ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeString $ T.pack x)
          `shouldBe` (Right TypeChecker.IpeString, Map.empty)

binaryOperationSpec :: Spec
binaryOperationSpec =
  describe "when dealing with binary operations" $ do
    prop "should type check adding two numbers" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Add (IpeNumber x) (IpeNumber y))
          `shouldBe` (Right TypeChecker.IpeNumber, Map.empty)

    prop "should not type check adding a number and something else" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Add (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` (Left "can't add Number and String. I can only add two Numbers.", Map.empty)

    prop "should type check subtracting two numbers" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Subtract (IpeNumber x) (IpeNumber y))
          `shouldBe` (Right TypeChecker.IpeNumber, Map.empty)

    prop "should not type check subtracting a number and something else" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Subtract (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` (Left "can't subtract Number and String. I can only subtract two Numbers.", Map.empty)

    prop "should type check dividing two numbers" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Divide (IpeNumber x) (IpeNumber y))
          `shouldBe` (Right TypeChecker.IpeNumber, Map.empty)

    prop "should not type check dividing a number and something else" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Divide (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` (Left "can't divide String and Number. I can only divide two Numbers.", Map.empty)

    prop "should type check multiplying two numbers" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Multiply (IpeNumber x) (IpeNumber y))
          `shouldBe` (Right TypeChecker.IpeNumber, Map.empty)

    prop "should not type check multiplying a number and something else" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Multiply (IpeString $ T.pack x) (IpeNumber y))
          `shouldBe` (Left "can't multiply String and Number. I can only multiply two Numbers.", Map.empty)

    prop "should type check exponentiation with two numbers" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeNumber y))
          `shouldBe` (Right TypeChecker.IpeNumber, Map.empty)

    prop "should type check exponentiation with a number and something else" $
      \x y ->
        TypeChecker.run Ipe.TypeChecker.Expression.typeCheck (IpeBinaryOperation Exponentiation (IpeNumber x) (IpeString $ T.pack y))
          `shouldBe` (Left "can't potentiate Number and String. I can only potentiate two Numbers.", Map.empty)

    prop "should type check basic pipe right" $
      \x ->
        let initialState = Map.singleton ([], "pipeRightFn") (TypeChecker.IpeFunction TypeChecker.IpeNumber TypeChecker.IpeNumber)
         in TypeChecker.runWith
              initialState
              Ipe.TypeChecker.Expression.typeCheck
              ( IpeBinaryOperation
                  PipeRight
                  (IpeNumber x)
                  ( IpeFunctionCallOrValue
                      ( FunctionCallOrValue
                          { functionCallOrValuePath = [],
                            functionCallOrValueName = "pipeRightFn",
                            functionCallOrValueRecordAccessors = [],
                            functionCallOrValueArguments = []
                          }
                      )
                  )
              )
              `shouldBe` ( Right TypeChecker.IpeNumber,
                           initialState
                         )

    prop "should type check basic pipe left" $
      \x ->
        let initialState = Map.singleton ([], "pipeLeftFn") (TypeChecker.IpeFunction TypeChecker.IpeNumber TypeChecker.IpeNumber)
         in TypeChecker.runWith
              initialState
              Ipe.TypeChecker.Expression.typeCheck
              ( IpeBinaryOperation
                  PipeLeft
                  ( IpeFunctionCallOrValue
                      ( FunctionCallOrValue
                          { functionCallOrValuePath = [],
                            functionCallOrValueName = "pipeLeftFn",
                            functionCallOrValueRecordAccessors = [],
                            functionCallOrValueArguments = []
                          }
                      )
                  )
                  (IpeNumber x)
              )
              `shouldBe` ( Right TypeChecker.IpeNumber,
                           initialState
                         )

    prop "should not type check basic pipe right with invalid argument type" $
      \x ->
        let initialState = Map.singleton ([], "pipeRightFn") (TypeChecker.IpeFunction TypeChecker.IpeString TypeChecker.IpeNumber)
         in TypeChecker.runWith
              initialState
              Ipe.TypeChecker.Expression.typeCheck
              ( IpeBinaryOperation
                  PipeRight
                  (IpeNumber x)
                  ( IpeFunctionCallOrValue
                      ( FunctionCallOrValue
                          { functionCallOrValuePath = [],
                            functionCallOrValueName = "pipeRightFn",
                            functionCallOrValueRecordAccessors = [],
                            functionCallOrValueArguments = []
                          }
                      )
                  )
              )
              `shouldBe` ( Left "can't pipe Number into a function that expects String.",
                           initialState
                         )

    prop "should not type check basic pipe left with invalid argument type" $
      \x ->
        let initialState = Map.singleton ([], "pipeLeftFn") (TypeChecker.IpeFunction TypeChecker.IpeString TypeChecker.IpeNumber)
         in TypeChecker.runWith
              initialState
              Ipe.TypeChecker.Expression.typeCheck
              ( IpeBinaryOperation
                  PipeLeft
                  ( IpeFunctionCallOrValue
                      ( FunctionCallOrValue
                          { functionCallOrValuePath = [],
                            functionCallOrValueName = "pipeLeftFn",
                            functionCallOrValueRecordAccessors = [],
                            functionCallOrValueArguments = []
                          }
                      )
                  )
                  (IpeNumber x)
              )
              `shouldBe` ( Left "can't pipe Number into a function that expects String.",
                           initialState
                         )

-- TODO - Add tests for FunctionCallOrValue
