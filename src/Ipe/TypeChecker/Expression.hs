{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.Expression (Type (..), run, runWith) where

import qualified Control.Monad
import Control.Monad.Trans.Except (throwE)
import qualified Data.List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Grammar (Expression (..), FunctionCallOrValue (..), IpeBinaryOperator (..), IpeFunctionBody (..), IpeMatchPattern (..))
import Ipe.TypeChecker.Utils
  ( Error (..),
    Scheme (..),
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
  | FiniteTVarListCases [TVarCases]
  | NoTVarCases

inferHelper :: TypeEnv -> Expression -> TypeInferenceMonad (Substitution, Type)
inferHelper env (IpeBinaryOperation operator exp1 exp2) = do
  (sub1, type1) <- inferHelper env exp1
  (sub2, type2) <- inferHelper (apply sub1 env) exp2
  case (operator, type1, type2) of
    (Add, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Add, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Add, TVar _, TVar _) -> do
      sub3 <- mostGeneralUnifier TNum (apply sub2 type2)
      sub4 <- mostGeneralUnifier TNum (apply sub3 type1)
      let composed = sub4 `compose` sub3 `compose` sub2
      return (composed, TNum)
    (Add, _, _) -> throwE $ InvalidOperation Add type1 type2
    (Subtract, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Subtract, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Subtract, TVar _, TVar _) -> do
      sub3 <- mostGeneralUnifier TNum (apply sub2 type2)
      sub4 <- mostGeneralUnifier TNum (apply sub3 type1)
      let composed = sub4 `compose` sub3 `compose` sub2
      return (composed, TNum)
    (Subtract, _, _) -> throwE $ InvalidOperation Subtract type1 type2
    (Divide, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Divide, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Divide, TVar _, TVar _) -> do
      sub3 <- mostGeneralUnifier TNum (apply sub2 type2)
      sub4 <- mostGeneralUnifier TNum (apply sub3 type1)
      let composed = sub4 `compose` sub3 `compose` sub2
      return (composed, TNum)
    (Divide, _, _) -> throwE $ InvalidOperation Divide type1 type2
    (Multiply, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Multiply, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Multiply, TVar _, TVar _) -> do
      sub3 <- mostGeneralUnifier TNum (apply sub2 type2)
      sub4 <- mostGeneralUnifier TNum (apply sub3 type1)
      let composed = sub4 `compose` sub3 `compose` sub2
      return (composed, TNum)
    (Multiply, _, _) -> throwE $ InvalidOperation Multiply type1 type2
    (Exponentiation, TNum, _) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type1) (apply sub2 type2)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type2)
    (Exponentiation, _, TNum) -> do
      sub3 <- mostGeneralUnifier (apply sub2 type2) (apply sub2 type1)
      return (sub3 `compose` sub2, apply (sub3 `compose` sub2) type1)
    (Exponentiation, TVar _, TVar _) -> do
      sub3 <- mostGeneralUnifier TNum (apply sub2 type2)
      sub4 <- mostGeneralUnifier TNum (apply sub3 type1)
      let composed = sub4 `compose` sub3 `compose` sub2
      return (composed, TNum)
    (Exponentiation, _, _) -> throwE $ InvalidOperation Exponentiation type1 type2
    (PipeRight, pipeInput, TFun fnInput fnOutput) -> do
      sub <- mostGeneralUnifier pipeInput fnInput
      return (sub `compose` sub2 `compose` sub1, fnOutput)
    (PipeRight, pipeInput, TVar pipeVar) -> do
      output <- newTypeVar "a"
      let sub = Map.singleton pipeVar (TFun pipeInput output)
      return (sub `compose` sub2 `compose` sub1, output)
    (PipeRight, _, _) -> throwE $ InvalidOperation PipeRight type1 type2
    (PipeLeft, TFun fnInput fnOutput, pipeInput) -> do
      sub <- mostGeneralUnifier pipeInput fnInput
      return (sub `compose` sub2 `compose` sub1, fnOutput)
    (PipeLeft, TVar pipeVar, pipeInput) -> do
      output <- newTypeVar "a"
      let sub = Map.singleton pipeVar (TFun pipeInput output)
      return (sub `compose` sub2 `compose` sub1, output)
    (PipeLeft, _, _) -> throwE $ InvalidOperation PipeLeft type1 type2
inferHelper _ (IpeNumber _) = return (Map.empty, TNum)
inferHelper env (IpeMatch matchExpr branches) = do
  (sub1, type1) <- inferHelper env matchExpr

  case type1 of
    TFun _ _ -> throwE PatternMatchOnFunction
    TRec _ -> throwE PatternMatchOnRecord
    TList innerType -> do
      returnT <- newTypeVar "a"

      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ -> do
                      handleAttributions
                        sub1
                        currEnv
                        branchAttributions
                        branchExpr
                        returnType
                        InfiniteCases
                IpeVariablePattern varName -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ -> do
                      let TypeEnv env' = currEnv
                      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] TNum) env'
                      handleAttributions
                        sub1
                        envWithVarName
                        branchAttributions
                        branchExpr
                        returnType
                        InfiniteCases
                IpeCustomTypePattern {} -> throwE InvalidTypeForPatternMatch
                IpeLiteralStringPattern _ -> throwE InvalidTypeForPatternMatch
                IpeLiteralListPattern pat ->
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases cases -> do
                      mapM_
                        ( \matchedCase ->
                            Control.Monad.when
                              ( encompasses
                                  (IpeLiteralListPattern matchedCase)
                                  (IpeLiteralListPattern pat)
                              )
                              $ throwE DuplicatePatternMatch
                        )
                        cases

                      finalEnv <-
                        Control.Monad.foldM
                          ( \acc curr ->
                              case (curr, innerType) of
                                (IpeWildCardPattern, _) -> return acc
                                (IpeVariablePattern varName, _) -> do
                                  let TypeEnv env' = acc
                                  let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] innerType) env'

                                  return envWithVarName
                                (IpeCustomTypePattern {}, _) -> throwE InvalidTypeForPatternMatch
                                (IpeLiteralNumberPattern _, TNum) -> return acc
                                (IpeLiteralStringPattern _, TStr) -> return acc
                                (IpeLiteralListPattern _, TList _) -> throwE InvalidTypeForPatternMatch
                                _ -> throwE InvalidTypeForPatternMatch
                          )
                          env
                          pat

                      handleAttributions sub1 finalEnv branchAttributions branchExpr returnType (FiniteCases (pat : cases))
                IpeLiteralNumberPattern _ ->
                  throwE InvalidTypeForPatternMatch
          )
          (apply sub1 env, returnT, FiniteCases [])
          branches

      case handledCases of
        FiniteCases _ -> throwE MissingPatternMatchCases
        InfiniteCases -> return (Map.empty, returnType)
    TVar tvar -> do
      returnT <- newTypeVar "a"

      (_, returnType, handledCases) <-
        Control.Monad.foldM
          (handleTVarPatternBranch (TVar tvar) sub1)
          (apply sub1 env, returnT, NoTVarCases)
          branches

      case handledCases of
        FiniteTVarNumCases _ -> throwE MissingPatternMatchCases
        FiniteTVarStrCases _ -> throwE MissingPatternMatchCases
        FiniteTVarCustomCases _ _ -> throwE MissingPatternMatchCases
        _ -> return (Map.empty, returnType)
    TNum -> do
      returnT <- newTypeVar "a"
      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ -> do
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteCases
                IpeVariablePattern varName -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ -> do
                      let TypeEnv env' = currEnv
                      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] TNum) env'
                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteCases
                IpeCustomTypePattern {} -> throwE InvalidTypeForPatternMatch
                IpeLiteralStringPattern _ -> throwE InvalidTypeForPatternMatch
                IpeLiteralListPattern _ -> throwE InvalidTypeForPatternMatch
                IpeLiteralNumberPattern pat ->
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases cases ->
                      if pat `elem` cases
                        then throwE DuplicatePatternMatch
                        else handleAttributions sub1 currEnv branchAttributions branchExpr returnType (FiniteCases $ pat : cases)
          )
          (apply sub1 env, returnT, FiniteCases [])
          branches

      case handledCases of
        FiniteCases _ -> throwE MissingPatternMatchCases
        InfiniteCases -> return (Map.empty, returnType)
    TStr -> do
      returnT <- newTypeVar "a"
      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ ->
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteCases
                IpeVariablePattern varName -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ -> do
                      let TypeEnv env' = currEnv
                      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] TStr) env'

                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteCases
                IpeCustomTypePattern {} -> throwE InvalidTypeForPatternMatch
                IpeLiteralStringPattern pat ->
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases cases ->
                      if pat `elem` cases
                        then throwE DuplicatePatternMatch
                        else handleAttributions sub1 currEnv branchAttributions branchExpr returnType (FiniteCases $ pat : cases)
                IpeLiteralListPattern _ -> throwE InvalidTypeForPatternMatch
                IpeLiteralNumberPattern _ -> throwE InvalidTypeForPatternMatch
          )
          (apply sub1 env, returnT, FiniteCases [])
          branches

      case handledCases of
        FiniteCases _ -> throwE MissingPatternMatchCases
        InfiniteCases -> return (Map.empty, returnType)
    TCustom name typeVars constructors -> do
      returnT <- newTypeVar "a"

      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ ->
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteCases
                IpeVariablePattern varName -> do
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases _ -> do
                      let TypeEnv env' = currEnv
                      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] (TCustom name typeVars constructors)) env'

                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteCases
                IpeCustomTypePattern path constructorName variables ->
                  case handledCases of
                    InfiniteCases -> throwE PatternMatchOnHandledPatternMatch
                    FiniteCases matchedConstructors -> do
                      let patternConstructorName = T.unpack $ T.intercalate "." (path ++ [constructorName])
                      case customTypeFromConstructorName patternConstructorName currEnv of
                        Nothing -> throwE $ ConstructorNotFound patternConstructorName
                        Just (otherName, _, otherRequiredConstructors, otherRequiredArgs)
                          | otherName /= name -> throwE InvalidTypeForPatternMatch
                          | length variables /= length otherRequiredArgs -> throwE $ InvalidNumberOfArguments (length variables) (length otherRequiredArgs)
                          | patternConstructorName `elem` matchedConstructors -> throwE DuplicatePatternMatch
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
                IpeLiteralStringPattern _ -> throwE InvalidTypeForPatternMatch
                IpeLiteralListPattern _ -> throwE InvalidTypeForPatternMatch
                IpeLiteralNumberPattern _ -> throwE InvalidTypeForPatternMatch
          )
          (apply sub1 env, returnT, FiniteCases [])
          branches

      case handledCases of
        FiniteCases _ -> throwE MissingPatternMatchCases
        InfiniteCases -> return (Map.empty, returnType)
inferHelper _ (IpeString _) = return (Map.empty, TStr)
inferHelper (TypeEnv env) (IpeFunctionCallOrValue (FunctionCallOrValue path name recordPath args)) =
  case Map.lookup fullName env of
    Nothing -> throwE $ UnboundVariable fullName
    Just valType -> do
      t <- instantiate valType
      typeToUse <- findRecordEnd t recordPath
      (_, finalT) <- Control.Monad.foldM evalFunction (Map.empty, typeToUse) args

      return (Map.empty, finalT)
      where
        findRecordEnd :: Type -> [Text] -> TypeInferenceMonad Type
        findRecordEnd inputType [] = return inputType
        findRecordEnd (TRec fields) currPath = do
          let (names, types) = unzip fields
          let maybeIndex = Data.List.elemIndex (T.unpack $ head currPath) names
          case maybeIndex of
            Nothing -> throwE $ MissingRecordField fields (T.unpack (head currPath))
            Just index -> findRecordEnd (types !! index) (tail currPath)
        findRecordEnd inputType _ = return inputType

        evalFunction (previousSub, type_) argExpr = do
          (argSub, argType) <- inferHelper (TypeEnv env) argExpr
          case type_ of
            TFun inputType outputType -> do
              sub <- mostGeneralUnifier inputType argType
              return (sub `compose` argSub `compose` previousSub, apply sub outputType)
            _ -> throwE $ TooManyArguments type_ argType
  where
    fullName = T.unpack (T.intercalate "." (path ++ [name]))
inferHelper _ (IpeFunction [] _) = throwE NoArguments
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
inferHelper env (IpeList exprs) = do
  newVar <- newTypeVar "a"

  (_, finalType) <-
    foldr
      ( \expr currMonad -> do
          (oldSub, _) <- currMonad
          (newSub, newType) <- inferHelper (apply oldSub env) expr
          sub <- mostGeneralUnifier newVar newType
          return (sub `compose` newSub, apply sub newVar)
      )
      (return (Map.empty, TList newVar))
      exprs

  return (Map.empty, TList finalType)

encompasses :: IpeMatchPattern -> IpeMatchPattern -> Bool
encompasses IpeWildCardPattern _ = True
encompasses (IpeVariablePattern _) _ = True
encompasses (IpeLiteralListPattern innerPatterns1) (IpeLiteralListPattern innerPatterns2) =
  length innerPatterns1 == length innerPatterns2
    && and (zipWith encompasses innerPatterns1 innerPatterns2)
encompasses p1 p2 = p1 == p2

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
        InfiniteTVarCases -> throwE PatternMatchOnHandledPatternMatch
        FiniteTVarStrCases _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarNumCases _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarCustomCases _ _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarListCases _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        NoTVarCases ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
    IpeVariablePattern varName -> do
      let TypeEnv env' = currEnv
      let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] branchType) env'

      case handledCases of
        InfiniteTVarCases -> throwE PatternMatchOnHandledPatternMatch
        NoTVarCases ->
          handleAttributions initialSub envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarStrCases _ ->
          handleAttributions initialSub envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarCustomCases _ _ ->
          handleAttributions initialSub currEnv branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarNumCases _ ->
          handleAttributions initialSub envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
        FiniteTVarListCases _ ->
          handleAttributions initialSub envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
    IpeCustomTypePattern path name args -> do
      let constructorName = T.unpack $ T.intercalate "." (path ++ [name])

      case customTypeFromConstructorName constructorName currEnv of
        Nothing -> throwE $ ConstructorNotFound constructorName
        Just (parentName, parentType, requiredConstructors, requiredArgs) ->
          if length args /= length requiredArgs
            then throwE $ InvalidNumberOfArguments (length args) (length requiredArgs)
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
                InfiniteTVarCases -> throwE PatternMatchOnHandledPatternMatch
                FiniteTVarStrCases _ -> throwE InvalidTypeForPatternMatch
                FiniteTVarNumCases _ -> throwE InvalidTypeForPatternMatch
                FiniteTVarCustomCases previousName matchedConstructors
                  | previousName /= parentName -> throwE InvalidTypeForPatternMatch
                  | constructorName `elem` matchedConstructors -> throwE DuplicatePatternMatch
                  | length matchedConstructors == length requiredConstructors - 1 -> handleAttributions newSub (apply newSub newEnv) branchAttributions branchExpr (apply newSub returnType) InfiniteTVarCases
                  | otherwise -> handleAttributions newSub (apply newSub newEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarCustomCases parentName (constructorName : matchedConstructors))
                FiniteTVarListCases _ -> throwE InvalidTypeForPatternMatch
                NoTVarCases ->
                  handleAttributions newSub (apply newSub newEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarCustomCases parentName [constructorName])
    IpeLiteralStringPattern pat ->
      case handledCases of
        FiniteTVarStrCases cases ->
          if pat `elem` cases
            then throwE DuplicatePatternMatch
            else do
              sub1 <- mostGeneralUnifier branchType TStr
              let newSub = sub1 `compose` initialSub

              handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarStrCases $ pat : cases)
        NoTVarCases -> do
          sub1 <- mostGeneralUnifier branchType TStr
          let newSub = sub1 `compose` initialSub

          handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarStrCases [pat])
        FiniteTVarNumCases _ -> throwE InvalidTypeForPatternMatch
        FiniteTVarCustomCases _ _ -> throwE InvalidTypeForPatternMatch
        FiniteTVarListCases _ -> throwE InvalidTypeForPatternMatch
        InfiniteTVarCases -> throwE PatternMatchOnHandledPatternMatch
    IpeLiteralNumberPattern pat ->
      case handledCases of
        FiniteTVarNumCases cases ->
          if pat `elem` cases
            then throwE DuplicatePatternMatch
            else do
              sub1 <- mostGeneralUnifier branchType TNum
              let newSub = sub1 `compose` initialSub

              handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarNumCases $ pat : cases)
        NoTVarCases -> do
          sub1 <- mostGeneralUnifier branchType TNum
          let newSub = sub1 `compose` initialSub

          handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarNumCases [pat])
        FiniteTVarStrCases _ -> throwE InvalidTypeForPatternMatch
        FiniteTVarCustomCases _ _ -> throwE InvalidTypeForPatternMatch
        FiniteTVarListCases _ -> throwE InvalidTypeForPatternMatch
        InfiniteTVarCases -> throwE PatternMatchOnHandledPatternMatch
    IpeLiteralListPattern _ -> case handledCases of
      InfiniteTVarCases -> throwE PatternMatchOnHandledPatternMatch
      FiniteTVarStrCases _ -> throwE InvalidTypeForPatternMatch
      FiniteTVarNumCases _ -> throwE InvalidTypeForPatternMatch
      FiniteTVarCustomCases _ _ -> throwE InvalidTypeForPatternMatch
      FiniteTVarListCases _ -> do
        listInnerType <- newTypeVar "a"
        sub1 <- mostGeneralUnifier branchType (TList listInnerType)
        let newSub = sub1 `compose` initialSub

        handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarListCases [])
      NoTVarCases -> do
        listInnerType <- newTypeVar "a"
        sub1 <- mostGeneralUnifier branchType (TList listInnerType)
        let newSub = sub1 `compose` initialSub

        handleAttributions newSub (apply newSub currEnv) branchAttributions branchExpr (apply newSub returnType) (FiniteTVarListCases [])

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

run :: Expression -> Either Error Type
run = runWith Map.empty

runWith :: Map.Map String Type -> Expression -> Either Error Type
runWith initialVars expression = runMonad (infer env expression)
  where
    env = Map.mapWithKey (\k -> Scheme [k]) initialVars
