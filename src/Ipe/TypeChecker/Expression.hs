{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.Expression (Type (..), run, runWith) where

import qualified Control.Monad
import Control.Monad.Trans.Except (throwE)
import qualified Data.List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Grammar (Expression (..), FunctionCallOrValue (..), IpeBinaryOperator (..), IpeFunctionBody (..), IpeMatchPattern (..))
import Ipe.TypeChecker
  ( Scheme (..),
    Substitution,
    Type (..),
    TypeEnv (..),
    TypeInferenceMonad,
    apply,
    compose,
    customTypeFromConstructorName,
    generalize,
    instantiate,
    mostGeneralUnifier,
    newTypeVar,
    remove,
    runMonad,
  )

data HandledCases a = InfiniteCases | FiniteCases [a]

data TVarCases
  = InfiniteTVarCases
  | FiniteTVarStrCases [Text]
  | FiniteTVarNumCases [Float]
  | FiniteTVarCustomCases String [String]
  | NoTVarCases

inferHelper :: TypeEnv -> Expression -> TypeInferenceMonad (Substitution, Type)
inferHelper env (IpeBinaryOperation operator exp1 exp2) = do
  (sub1, type1) <- inferHelper env exp1
  (sub2, type2) <- inferHelper (apply sub1 env) exp2
  let errorStr functionName expectedType = "can't " ++ functionName ++ " " ++ show type1 ++ " and " ++ show type2 ++ ". I can only " ++ functionName ++ " two " ++ expectedType ++ "."
  case (operator, type1, type2) of
    (Add, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Add, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Add, _, _) -> throwE $ errorStr "add" "Numbers"
    (Subtract, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Subtract, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Subtract, _, _) -> throwE $ errorStr "subtract" "Numbers"
    (Divide, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Divide, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Divide, _, _) -> throwE $ errorStr "divide" "Numbers"
    (Multiply, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Multiply, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Multiply, _, _) -> throwE $ errorStr "multiply" "Numbers"
    (Exponentiation, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Exponentiation, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Exponentiation, _, _) -> throwE $ errorStr "potentiate" "Numbers"
    (PipeRight, pipeInput, TFun fnInput fnOutput) -> do
      sub <- mostGeneralUnifier pipeInput fnInput
      return (sub `compose` sub2 `compose` sub1, fnOutput)
    (PipeRight, _, _) -> throwE "can't pipe right. I can only pipe right if the right side is a function."
    (PipeLeft, TFun fnInput fnOutput, pipeInput) -> do
      sub <- mostGeneralUnifier pipeInput fnInput
      return (sub `compose` sub2 `compose` sub1, fnOutput)
    (PipeLeft, _, _) -> throwE "can't pipe left. I can only pipe left if the left side is a function."
inferHelper _ (IpeNumber _) = return (Map.empty, TNum)
inferHelper env (IpeMatch matchExpr branches) = do
  (sub1, type1) <- inferHelper env matchExpr

  case type1 of
    TFun _ _ -> throwE "can't pattern match on a function. You need to apply all arguments to it first."
    TRec _ -> throwE "can't pattern match on a record. If you want to access a field, use the dot operator."
    TVar tvar -> do
      returnT <- newTypeVar "a"

      (_, returnType, handledCases) <-
        Control.Monad.foldM
          (handleTVarPatternBranch (TVar tvar) sub1)
          (apply sub1 env, returnT, NoTVarCases)
          branches

      case handledCases of
        FiniteTVarNumCases _ -> throwE "can't pattern match on a number with a finite pattern match without matching all possible cases."
        FiniteTVarStrCases _ -> throwE "can't pattern match on a string with a finite pattern match without matching all possible cases."
        FiniteTVarCustomCases typeName _ -> throwE $ "can't pattern match on a custom type (" ++ typeName ++ ") with a finite pattern match without matching all possible cases."
        _ -> return (Map.empty, returnType)
    TNum -> do
      returnT <- newTypeVar "a"
      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern -> do
                  case handledCases of
                    InfiniteCases -> throwE "can't pattern match on a number with an infinite pattern match after an infinite pattern match."
                    FiniteCases _ -> do
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteCases
                IpeVariablePattern varName -> do
                  case handledCases of
                    InfiniteCases -> throwE "can't pattern match on a number with an infinite pattern match after an infinite pattern match."
                    FiniteCases _ -> do
                      let TypeEnv env' = currEnv
                      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] TNum) env'
                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteCases
                IpeCustomTypePattern {} -> throwE "can't pattern match on a number with a custom type pattern."
                IpeLiteralStringPattern _ -> throwE "can't pattern match on a number with a string pattern."
                IpeLiteralNumberPattern pat ->
                  case handledCases of
                    InfiniteCases -> throwE "can't pattern match on a number with a number pattern after an infinite pattern match."
                    FiniteCases cases ->
                      if pat `elem` cases
                        then throwE $ "number " ++ show pat ++ " is already pattern matched."
                        else handleAttributions sub1 currEnv branchAttributions branchExpr returnType (FiniteCases $ pat : cases)
          )
          (apply sub1 env, returnT, FiniteCases [])
          branches

      case handledCases of
        FiniteCases _ -> throwE "can't pattern match on a number with a finite pattern match without matching all possible cases."
        InfiniteCases -> return (Map.empty, returnType)
    TStr -> do
      returnT <- newTypeVar "a"
      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern -> do
                  case handledCases of
                    InfiniteCases -> throwE "can't pattern match on a string with an infinite pattern match after an infinite pattern match."
                    FiniteCases _ ->
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteCases
                IpeVariablePattern varName -> do
                  case handledCases of
                    InfiniteCases -> throwE "can't pattern match on a string with an infinite pattern match after an infinite pattern match."
                    FiniteCases _ -> do
                      let TypeEnv env' = currEnv
                      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] TStr) env'

                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteCases
                IpeCustomTypePattern {} -> throwE "can't pattern match on a string with a custom type pattern."
                IpeLiteralStringPattern pat ->
                  case handledCases of
                    InfiniteCases -> throwE "can't pattern match on a string with a string pattern after an infinite pattern match."
                    FiniteCases cases ->
                      if pat `elem` cases
                        then throwE $ "string " ++ show pat ++ " is already pattern matched."
                        else handleAttributions sub1 currEnv branchAttributions branchExpr returnType (FiniteCases $ pat : cases)
                IpeLiteralNumberPattern _ -> throwE "can't pattern match on a string with a number pattern."
          )
          (apply sub1 env, returnT, FiniteCases [])
          branches

      case handledCases of
        FiniteCases _ -> throwE "can't pattern match on a string with a finite pattern match without matching all possible cases."
        InfiniteCases -> return (Map.empty, returnType)
    TCustom name typeVars constructors -> do
      returnT <- newTypeVar "a"

      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern -> do
                  case handledCases of
                    InfiniteCases -> throwE $ "can't pattern match on a custom type (" ++ name ++ ") with an infinite pattern match after an infinite pattern match."
                    FiniteCases _ ->
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteCases
                IpeVariablePattern varName -> do
                  case handledCases of
                    InfiniteCases -> throwE $ "can't pattern match on a custom type (" ++ name ++ ") with an infinite pattern match after an infinite pattern match."
                    FiniteCases _ -> do
                      let TypeEnv env' = currEnv
                      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] (TCustom name typeVars constructors)) env'

                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteCases
                IpeCustomTypePattern path constructorName variables ->
                  case handledCases of
                    InfiniteCases -> throwE $ "can't pattern match on a custom type (" ++ name ++ ") with a custom type (" ++ name ++ ") pattern after an infinite pattern match."
                    FiniteCases matchedConstructors -> do
                      let patternConstructorName = T.unpack $ T.intercalate "." (path ++ [constructorName])
                      case customTypeFromConstructorName patternConstructorName currEnv of
                        Nothing -> throwE $ "constructor " ++ patternConstructorName ++ " is not defined."
                        Just (otherName, _, otherRequiredConstructors, otherRequiredArgs)
                          | otherName /= name -> throwE $ "can't pattern match on a custom type (" ++ name ++ ") with another custom type (" ++ otherName ++ ") pattern."
                          | length variables /= length otherRequiredArgs -> throwE $ "expected " ++ show (length otherRequiredArgs) ++ " arguments to match on constructor " ++ patternConstructorName ++ ", but got " ++ show (length variables) ++ "."
                          | patternConstructorName `elem` matchedConstructors -> throwE $ "constructor " ++ patternConstructorName ++ " is already pattern matched."
                          | otherwise -> do
                              let newEnv =
                                    foldr
                                      ( \(argName, argType) (TypeEnv e) ->
                                          TypeEnv $
                                            Map.insert
                                              (T.unpack argName)
                                              (Scheme [T.unpack argName] argType)
                                              e
                                      )
                                      currEnv
                                      $ zip variables otherRequiredArgs
                              let newHandledCases =
                                    if length matchedConstructors == length otherRequiredConstructors - 1
                                      then InfiniteCases
                                      else FiniteCases $ patternConstructorName : matchedConstructors

                              (e, t, h) <- handleAttributions sub1 newEnv branchAttributions branchExpr returnType newHandledCases

                              return (e, t, h)
                IpeLiteralStringPattern _ -> throwE $ "can't pattern match on a custom type (" ++ name ++ ") with a string pattern."
                IpeLiteralNumberPattern _ -> throwE $ "can't pattern match on a custom type (" ++ name ++ ") with a number pattern."
          )
          (apply sub1 env, returnT, FiniteCases [])
          branches

      case handledCases of
        FiniteCases _ -> throwE $ "can't pattern match on a custom type (" ++ name ++ ") with a finite pattern match without matching all possible cases."
        InfiniteCases -> return (Map.empty, returnType)
inferHelper _ (IpeString _) = return (Map.empty, TStr)
inferHelper (TypeEnv env) (IpeFunctionCallOrValue (FunctionCallOrValue path name recordPath args)) =
  case Map.lookup fullName env of
    Nothing -> throwE $ "unbound variable: " ++ fullName
    Just valType -> do
      t <- instantiate valType
      typeToUse <- findRecordEnd t recordPath
      (finalSub, finalT) <- Control.Monad.foldM evalFunction (Map.empty, typeToUse) args
      return (finalSub, finalT)
      where
        findRecordEnd :: Type -> [Text] -> TypeInferenceMonad Type
        findRecordEnd inputType [] = return inputType
        findRecordEnd (TRec fields) currPath = do
          let (names, types) = unzip fields
          let maybeIndex = Data.List.elemIndex (T.unpack $ head currPath) names
          case maybeIndex of
            Nothing -> throwE $ "record " ++ show (TRec fields) ++ " does not have a field named " ++ T.unpack (head currPath)
            Just index -> findRecordEnd (types !! index) (tail currPath)
        findRecordEnd inputType _ = return inputType

        evalFunction (previousSub, type_) argExpr = do
          (argSub, argType) <- inferHelper (TypeEnv env) argExpr
          case type_ of
            TFun inputType outputType -> do
              sub <- mostGeneralUnifier inputType argType
              return (sub `compose` argSub `compose` previousSub, apply sub outputType)
            _ -> throwE $ fullName ++ " is already a " ++ show type_ ++ ", so it can't be called as a function. You tried giving it a " ++ show argType ++ "."
  where
    fullName = T.unpack (T.intercalate "." (path ++ [name]))
inferHelper _ (IpeFunction [] _) = throwE "function must have at least one argument"
inferHelper env (IpeFunction args (IpeFunctionBody attrs fnReturn)) = do
  argTypeVars <-
    foldr
      ( \_ vars -> do
          newVar <- newTypeVar "a"
          existingVars <- vars
          return $ newVar : existingVars
      )
      (return [])
      args
  let (TypeEnv env') =
        foldr
          ( \currentArg currentEnv ->
              remove currentEnv (T.unpack currentArg)
          )
          env
          args
  let schemes = zip (map T.unpack args) (map (Scheme []) argTypeVars)
  let env'' = TypeEnv (Map.union env' (Map.fromList schemes))

  (_, envAfterAttributions) <-
    foldr
      ( \(attrName, attrExpr) currMonad -> do
          (oldSubsAndTypes, oldEnv) <- currMonad
          (newSub, newType, newEnv) <- inferAttribution attrName attrExpr oldEnv

          return ((newSub, newType) : oldSubsAndTypes, newEnv)
      )
      (return ([], env''))
      attrs

  (sub, type1) <- inferHelper envAfterAttributions fnReturn

  return (sub, apply sub (foldr TFun type1 argTypeVars))
inferHelper env (IpeRecord fields) = do
  (finalFields, _) <-
    foldr
      ( \(fieldName, fieldExpr) currMonad -> do
          (currFields, oldEnv) <- currMonad
          (newSub, newType) <- inferHelper oldEnv fieldExpr

          return ((T.unpack fieldName, newType) : currFields, apply newSub oldEnv)
      )
      (return ([], env))
      fields
  return (Map.empty, TRec finalFields)

handleTVarPatternBranch ::
  Type ->
  Substitution ->
  (TypeEnv, Type, TVarCases) ->
  (IpeMatchPattern, [(Text, Expression)], Expression) ->
  TypeInferenceMonad (TypeEnv, Type, TVarCases)
handleTVarPatternBranch branchType initialSub (currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) =
  case branchPattern of
    IpeWildCardPattern ->
      case handledCases of
        InfiniteTVarCases -> throwE "can't pattern match with an infinite pattern match after an infinite pattern match."
        FiniteTVarStrCases _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarNumCases _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarCustomCases _ _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        NoTVarCases ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
    IpeVariablePattern varName -> do
      let TypeEnv env' = currEnv
      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] branchType) env'

      case handledCases of
        InfiniteTVarCases -> throwE "can't pattern match with an infinite pattern match after an infinite pattern match."
        NoTVarCases ->
          handleAttributions initialSub envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarStrCases _ ->
          handleAttributions initialSub envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarCustomCases _ _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarNumCases _ ->
          handleAttributions initialSub envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
    IpeCustomTypePattern path name args -> do
      let constructorName = T.unpack $ T.intercalate "." (path ++ [name])

      case customTypeFromConstructorName constructorName currEnv of
        Nothing -> throwE $ "constructor " ++ constructorName ++ " is not defined."
        Just (parentName, parentType, requiredConstructors, requiredArgs) ->
          if length args /= length requiredArgs
            then throwE $ "expected " ++ show (length requiredArgs) ++ " arguments to match on constructor " ++ constructorName ++ ", but got " ++ show (length args) ++ "."
            else do
              branchSub <- mostGeneralUnifier parentType branchType

              let newEnv =
                    foldr
                      ( \(argName, argType) (TypeEnv env) ->
                          TypeEnv $
                            Map.insert
                              (T.unpack argName)
                              (Scheme [T.unpack argName] argType)
                              env
                      )
                      currEnv
                      $ zip args requiredArgs

              let newSub = branchSub `compose` initialSub

              case handledCases of
                InfiniteTVarCases -> throwE $ "can't pattern match on a " ++ parentName ++ " with a " ++ parentName ++ " pattern after an infinite pattern match."
                FiniteTVarStrCases _ -> throwE $ parentName ++ " type does not match the type from previous branches, which was String."
                FiniteTVarNumCases _ -> throwE $ parentName ++ " type does not match the type from previous branches, which was Number."
                FiniteTVarCustomCases previousName matchedConstructors
                  | previousName /= parentName -> throwE $ parentName ++ " type does not match the type from previous branches, which was " ++ previousName ++ "."
                  | constructorName `elem` matchedConstructors -> throwE $ "constructor " ++ constructorName ++ " is already pattern matched."
                  | length matchedConstructors == length requiredConstructors - 1 -> handleAttributions newSub (apply newSub newEnv) branchAttributions branchExpr (apply newSub returnType) InfiniteTVarCases
                  | otherwise -> handleAttributions newSub (apply newSub newEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarCustomCases parentName (constructorName : matchedConstructors))
                NoTVarCases ->
                  handleAttributions newSub (apply newSub newEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarCustomCases parentName [constructorName])
    IpeLiteralStringPattern pat ->
      case handledCases of
        FiniteTVarStrCases cases ->
          if pat `elem` cases
            then throwE $ "string " ++ show pat ++ " is already pattern matched."
            else do
              sub1 <- mostGeneralUnifier branchType TStr
              let newSub = sub1 `compose` initialSub

              handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarStrCases $ pat : cases)
        NoTVarCases -> do
          sub1 <- mostGeneralUnifier branchType TStr
          let newSub = sub1 `compose` initialSub

          handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarStrCases [pat])
        FiniteTVarNumCases _ ->
          throwE "String type does not match the type from previous branches, which was Number."
        FiniteTVarCustomCases typeName _ ->
          throwE $ "String type does not match the type from previous branches, which was " ++ typeName ++ "."
        InfiniteTVarCases -> throwE "can't pattern match on a String with a String pattern after an infinite pattern match."
    IpeLiteralNumberPattern pat ->
      case handledCases of
        FiniteTVarNumCases cases ->
          if pat `elem` cases
            then throwE $ "number " ++ show pat ++ " is already pattern matched."
            else do
              sub1 <- mostGeneralUnifier branchType TNum
              let newSub = sub1 `compose` initialSub

              handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarNumCases $ pat : cases)
        NoTVarCases -> do
          sub1 <- mostGeneralUnifier branchType TNum
          let newSub = sub1 `compose` initialSub

          handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarNumCases [pat])
        FiniteTVarStrCases _ ->
          throwE "Number type does not match the type from previous branches, which was String."
        FiniteTVarCustomCases typeName _ ->
          throwE $ "Number type does not match the type from previous branches, which was " ++ typeName ++ "."
        InfiniteTVarCases -> throwE "can't pattern match on a Number with a Number pattern after an infinite pattern match."

inferAttribution :: Text -> Expression -> TypeEnv -> TypeInferenceMonad (Substitution, Type, TypeEnv)
inferAttribution attributionName attributionExpr env = do
  (sub1, type1) <- inferHelper env attributionExpr
  let TypeEnv env' = remove env $ T.unpack attributionName
  let t' = generalize (apply sub1 env) type1
  let env'' = TypeEnv (Map.insert (T.unpack attributionName) t' env')

  return (sub1, type1, env'')

handleAttributions :: Substitution -> TypeEnv -> [(Text, Expression)] -> Expression -> Type -> a -> TypeInferenceMonad (TypeEnv, Type, a)
handleAttributions sub env attributions returnExpression returnType newHandledCases = do
  (_, envAfterAttributions) <-
    foldl
      ( \currMonad (attrName, attrExpr) -> do
          (currSub, oldEnv) <- currMonad
          (newSub, _, newEnv) <- inferAttribution attrName attrExpr oldEnv
          let composedSub = newSub `compose` currSub

          return (composedSub, apply composedSub newEnv)
      )
      (return (sub, env))
      attributions

  (sub2, type2) <- inferHelper envAfterAttributions returnExpression
  sub3 <- mostGeneralUnifier type2 returnType
  return (apply (sub3 `compose` sub2) envAfterAttributions, apply sub3 type2, newHandledCases)

infer :: Map.Map String Scheme -> Expression -> TypeInferenceMonad Type
infer env expression = do
  (subst, type_) <- inferHelper (TypeEnv env) expression
  return $ apply subst type_

run :: Expression -> Either String Type
run = runWith Map.empty

runWith :: Map.Map String Type -> Expression -> Either String Type
runWith initialVars expression = runMonad (infer env expression)
  where
    env = Map.mapWithKey (\k -> Scheme [k]) initialVars
