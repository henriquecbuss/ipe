{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.Expression (typeCheck) where

import qualified Control.Monad.Trans.Except as Except
import qualified Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar
import Ipe.TypeChecker (IpeType (..), TypeCheckingMonad)
import qualified Ipe.TypeChecker as TypeChecker

typeCheck :: Ipe.Grammar.Expression -> TypeCheckingMonad
typeCheck expr =
  case expr of
    Ipe.Grammar.IpeNumber _ -> return IpeNumber
    Ipe.Grammar.IpeString _ -> return IpeString
    Ipe.Grammar.IpeBinaryOperation operation left right -> do
      leftResult <- typeCheck left
      rightResult <- typeCheck right

      let checkSame operationName expectedType =
            if leftResult == expectedType && rightResult == expectedType
              then return expectedType
              else Except.throwE $ "can't " <> operationName <> " " <> T.pack (show leftResult) <> " and " <> T.pack (show rightResult) <> ". I can only " <> operationName <> " two " <> T.pack (show expectedType) <> "s."

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
          case rightResult of
            IpeFunction input output ->
              if input == leftResult
                then return output
                else Except.throwE $ "can't pipe " <> T.pack (show leftResult) <> " into a function that expects " <> T.pack (show input) <> "."
            _ ->
              Except.throwE $ "can only pipe into a function. Tried to pipe into a " <> T.pack (show rightResult) <> " instead."
        Ipe.Grammar.PipeLeft ->
          case leftResult of
            IpeFunction input output ->
              if input == rightResult
                then return output
                else Except.throwE $ "can't pipe " <> T.pack (show rightResult) <> " into a function that expects " <> T.pack (show input) <> "."
            _ ->
              Except.throwE $ "can only pipe into a function. Tried to pipe into a " <> T.pack (show rightResult) <> " instead."
    Ipe.Grammar.IpeMatch _ _ -> do
      Except.throwE "TODO: match not implemented"
    Ipe.Grammar.IpeFunctionCallOrValue (Ipe.Grammar.FunctionCallOrValue path name recordAccessors arguments) -> do
      value <- TypeChecker.get (path, name)
      case (value, recordAccessors, arguments) of
        (Just valueType, [], []) -> return valueType
        (Just (IpeRecord record), accessors, fnArgs) -> case (findTypeFromRecord record accessors, fnArgs) of
          (Left err, _) -> Except.throwE err
          (Right valueType, []) -> return valueType
          (Right (IpeFunction fnInput fnOutput), args) -> applyArgsToFunction fnInput fnOutput args
          (Right _, _ : _) -> Except.throwE "can't call a non-function"
        (Just (IpeFunction fnInput fnOutput), [], args) -> applyArgsToFunction fnInput fnOutput args
        (Just _, _ : _, _) -> Except.throwE "can't access fields on a non-record"
        (Just _, _, _ : _) -> Except.throwE "can't call a non-function"
        (Nothing, _, _) -> Except.throwE $ "Identifier " <> T.intercalate "." (path ++ [name]) <> " could not be found."
    Ipe.Grammar.IpeFunction _ _ ->
      Except.throwE "TODO: lambda function not implemented"

findTypeFromRecord :: Map Text IpeType -> [Text] -> Either Text IpeType
findTypeFromRecord record =
  Data.List.foldl
    ( \accum recordAccessor -> case accum of
        Right (IpeRecord currentRecord) -> case Map.lookup recordAccessor currentRecord of
          Nothing -> Left $ "record doesn't have a " <> recordAccessor <> " field."
          Just newType -> Right newType
        Left err -> Left err
        _ -> Left "can't access a field on a non-record type"
    )
    (Right $ IpeRecord record)

applyArgsToFunction :: IpeType -> IpeType -> [Ipe.Grammar.Expression] -> TypeCheckingMonad
applyArgsToFunction fnInput fnOutput [] = return $ IpeFunction fnInput fnOutput
applyArgsToFunction fnInput fnOutput [x] = do
  exprType <- typeCheck x

  if exprType == fnInput
    then return fnOutput
    else Except.throwE $ "Can't match expected type " <> T.pack (show fnInput) <> " with actual type " <> T.pack (show exprType) <> "."
applyArgsToFunction fnInput fnOutput (x : xs) = do
  exprType <- typeCheck x

  if exprType == fnInput
    then case fnOutput of
      IpeFunction newInput newOutput -> applyArgsToFunction newInput newOutput xs
      _ -> Except.throwE "can't call a non-function"
    else Except.throwE $ "can't match expected type " <> T.pack (show fnInput) <> " with actual type " <> T.pack (show exprType) <> "."
