{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeCheck (typeCheckExpression, runTypeCheckExpression) where

import qualified Control.Monad
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State.Lazy (State)
import qualified Control.Monad.Trans.State.Lazy as State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar

data IpeType
  = IpeNumber
  | IpeString
  | IpeFunction [IpeType] IpeType
  deriving (Eq)

instance Show IpeType where
  show IpeNumber = "Number"
  show IpeString = "String"
  show (IpeFunction _ _) = "Function"

type TypeCheckingMonad = ExceptT Text (State (Map Text IpeType)) IpeType

runTypeCheckExpression :: Ipe.Grammar.Expression -> (Either Text IpeType, Map Text IpeType)
runTypeCheckExpression expression =
  State.runState (Except.runExceptT (typeCheckExpression expression)) Map.empty

typeCheckExpression :: Ipe.Grammar.Expression -> TypeCheckingMonad
typeCheckExpression expr =
  case expr of
    Ipe.Grammar.IpeNumber _ -> return IpeNumber
    Ipe.Grammar.IpeString _ -> return IpeString
    Ipe.Grammar.IpeBinaryOperation operation left right -> do
      leftResult <- typeCheckExpression left
      rightResult <- typeCheckExpression right

      let checkSame operationName expectedType =
            if leftResult == expectedType && rightResult == expectedType
              then return expectedType
              else Except.throwE ("can't " <> operationName <> " " <> T.pack (show leftResult) <> " and " <> T.pack (show rightResult) <> ". I can only " <> operationName <> " two " <> T.pack (show expectedType) <> "s.")

      case operation of
        Ipe.Grammar.Add ->
          checkSame "add" IpeNumber
        Ipe.Grammar.Subtract ->
          checkSame "subtract" IpeNumber
        Ipe.Grammar.Divide ->
          checkSame "divide" IpeNumber
        Ipe.Grammar.Multiply ->
          checkSame "multiply" IpeNumber
        Ipe.Grammar.Exponentiation ->
          checkSame "potentiate" IpeNumber
        Ipe.Grammar.PipeRight ->
          Except.throwE "pipe right not implemented"
        Ipe.Grammar.PipeLeft ->
          Except.throwE "pipe left not implemented"
    Ipe.Grammar.IpeMatch matchExpression branches -> do
      Except.throwE "match not implemented"
    Ipe.Grammar.IpeFunctionCallOrValue _ -> do
      Except.throwE "function call or value not implemented"
    Ipe.Grammar.IpeFunction arguments body ->
      Except.throwE "lambda function not implemented"
