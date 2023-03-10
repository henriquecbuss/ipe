{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker (Type (..), run, runWith) where

-- \| This module uses Algorithm W, explained in https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf

import qualified Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get, put)
import qualified Data.Bifunctor
import qualified Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Grammar (Expression (..), FunctionCallOrValue (..), IpeBinaryOperator (..), IpeFunctionBody (..), IpeMatchPattern (..))

-- TYPES

class Types a where
  freeTypeVariables :: a -> Set.Set String
  apply :: Substitution -> a -> a

instance Types a => Types [a] where
  freeTypeVariables = foldr (Set.union . freeTypeVariables) Set.empty
  apply = map . apply

data Type
  = TVar String
  | TNum
  | TStr
  | TFun Type Type
  | TRec [(String, Type)]
  -- TODO - Better show implementation?
  deriving (Eq)

instance Show Type where
  show (TVar name) = name
  show TNum = "Number"
  show TStr = "String"
  show (TFun input output) = "(" ++ show input ++ " -> " ++ show output ++ ")"
  show (TRec []) = "{}"
  show (TRec fields) = "{ " ++ Data.List.intercalate ", " (map (\(name, t) -> name ++ ": " ++ show t) fields) ++ " }"

instance Types Type where
  freeTypeVariables (TVar var) = Set.singleton var
  freeTypeVariables TNum = Set.empty
  freeTypeVariables TStr = Set.empty
  freeTypeVariables (TFun input output) = Set.union (freeTypeVariables input) (freeTypeVariables output)
  freeTypeVariables (TRec fields) = Set.unions (map (freeTypeVariables . snd) fields)

  apply subs (TVar var) = case Map.lookup var subs of
    Nothing -> TVar var
    Just t -> t
  apply subs (TFun input output) = TFun (apply subs input) (apply subs output)
  apply _ TNum = TNum
  apply _ TStr = TStr
  apply subs (TRec fields) = TRec (map (Data.Bifunctor.second (apply subs)) fields)

data Scheme = Scheme [String] Type

instance Types Scheme where
  freeTypeVariables (Scheme vars t) = Set.difference (freeTypeVariables t) (Set.fromList vars)
  apply subs (Scheme vars t) = Scheme vars (apply (foldr Map.delete subs vars) t)

type Substitution = Map.Map String Type

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

instance Types TypeEnv where
  freeTypeVariables (TypeEnv env) = freeTypeVariables (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

data InferenceState = InferenceState {lastVarIndex :: Int, currentSubstitution :: Substitution}

type TypeInferenceMonad a = ExceptT String (State InferenceState) a

data HandledCases a = InfiniteCases | FiniteCases [a]

data TVarCases
  = InfiniteTVarCases
  | FiniteTVarStrCases [Text]
  | FiniteTVarNumCases [Float]
  | NoTVarCases

-- HELPER FUNCTIONS

compose :: Substitution -> Substitution -> Substitution
compose subs1 subs2 = Map.map (apply subs1) subs2 `Map.union` subs1

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where
    vars = Set.toList $ Set.difference (freeTypeVariables t) (freeTypeVariables env)

newTypeVar :: String -> TypeInferenceMonad Type
newTypeVar prefix = do
  currentState <- lift get
  lift $ put $ currentState {lastVarIndex = lastVarIndex currentState + 1}
  return $ TVar (prefix ++ show (lastVarIndex currentState))

instantiate :: Scheme -> TypeInferenceMonad Type
instantiate (Scheme vars t) = do
  nvars <- mapM (\_ -> newTypeVar "a") vars
  let s = Map.fromList (zip vars nvars)
  return $ apply s t

mostGeneralUnifier :: Type -> Type -> TypeInferenceMonad Substitution
mostGeneralUnifier (TFun input1 output1) (TFun input2 output2) = do
  sub1 <- mostGeneralUnifier input1 input2
  sub2 <- mostGeneralUnifier (apply sub1 output1) (apply sub1 output2)
  return (compose sub1 sub2)
mostGeneralUnifier (TVar var) t = varBind var t
mostGeneralUnifier t (TVar var) = varBind var t
mostGeneralUnifier TNum TNum = return Map.empty
mostGeneralUnifier TStr TStr = return Map.empty
mostGeneralUnifier (TRec fields1) (TRec fields2)
  | length fields1 == length fields2 = do
      let (names1, types1) = unzip fields1
      let (names2, types2) = unzip fields2
      if names1 == names2
        then do
          subs <- Control.Monad.zipWithM mostGeneralUnifier types1 types2
          return $ foldr compose Map.empty subs
        else throwE $ "can't match expected record\n\t" ++ show (TRec fields1) ++ "\nwith actual record\n\t" ++ show (TRec fields2)
  | otherwise = throwE $ "can't match expected record\n\t" ++ show (TRec fields1) ++ "\nwith actual record\n\t" ++ show (TRec fields2)
mostGeneralUnifier t1 t2 = throwE $ "can't match expected type\n\t" ++ show t1 ++ "\nwith actual type\n\t" ++ show t2

varBind :: String -> Type -> TypeInferenceMonad Substitution
varBind var t
  | t == TVar var = return Map.empty
  | var `Set.member` freeTypeVariables t = throwE $ "occurs check fails: " ++ var ++ " in " ++ show t
  | otherwise = return $ Map.singleton var t

-- MAIN FUNCTIONS

runMonad :: TypeInferenceMonad a -> Either String a
runMonad t =
  evalState (runExceptT t) initialInferenceState

initialInferenceState :: InferenceState
initialInferenceState = InferenceState {lastVarIndex = 0, currentSubstitution = Map.empty}

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
    -- TODO - Support custom types
    TFun _ _ -> throwE "can't pattern match on a function. You need to apply all arguments to it first."
    TRec _ -> throwE "can't pattern match on a record. If you want to access a field, use the dot operator."
    TVar tvar -> do
      returnT <- newTypeVar "a"

      (_, returnType, handledCases) <-
        Control.Monad.foldM
          ( \(currEnv, returnType, handledCases) (branchPattern, branchAttributions, branchExpr) ->
              case branchPattern of
                IpeWildCardPattern ->
                  case handledCases of
                    InfiniteTVarCases -> throwE "can't pattern match with an infinite pattern match after an infinite pattern match."
                    FiniteTVarStrCases _ ->
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteTVarCases
                    FiniteTVarNumCases _ ->
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteTVarCases
                    NoTVarCases ->
                      handleAttributions sub1 currEnv branchAttributions branchExpr returnType InfiniteTVarCases
                IpeVariablePattern varName -> do
                  let TypeEnv env' = currEnv
                  let envWithVarName = TypeEnv $ Map.insert (T.unpack varName) (Scheme [T.unpack varName] (TVar tvar)) env'

                  case handledCases of
                    InfiniteTVarCases -> throwE "can't pattern match with an infinite pattern match after an infinite pattern match."
                    NoTVarCases ->
                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
                    FiniteTVarStrCases _ ->
                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
                    FiniteTVarNumCases _ ->
                      handleAttributions sub1 envWithVarName branchAttributions branchExpr returnType InfiniteTVarCases
                IpeCustomTypePattern {} -> throwE "TODO: Implement custom type patterns"
                IpeLiteralStringPattern pat ->
                  case handledCases of
                    FiniteTVarStrCases cases ->
                      if pat `elem` cases
                        then throwE $ "number " ++ show pat ++ " is already pattern matched."
                        else handleAttributions sub1 currEnv branchAttributions branchExpr returnType (FiniteTVarStrCases $ pat : cases)
                    _ -> throwE "can't pattern match on a number with a number pattern after an infinite pattern match."
                IpeLiteralNumberPattern pat ->
                  case handledCases of
                    FiniteTVarNumCases cases ->
                      if pat `elem` cases
                        then throwE $ "number " ++ show pat ++ " is already pattern matched."
                        else handleAttributions sub1 currEnv branchAttributions branchExpr returnType (FiniteTVarNumCases $ pat : cases)
                    _ -> throwE "can't pattern match on a number with a number pattern after an infinite pattern match."
          )
          (apply sub1 env, returnT, NoTVarCases)
          branches

      case handledCases of
        FiniteTVarNumCases _ -> throwE "can't pattern match on a number with a finite pattern match without matching all possible cases."
        FiniteTVarStrCases _ -> throwE "can't pattern match on a string with a finite pattern match without matching all possible cases."
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
                    InfiniteCases -> throwE "can't pattern match on a string with a number pattern after an infinite pattern match."
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
inferHelper _ (IpeString _) = return (Map.empty, TStr)
inferHelper (TypeEnv env) (IpeFunctionCallOrValue (FunctionCallOrValue path name recordPath args)) =
  case Map.lookup fullName env of
    Nothing -> throwE $ "unbound variable: " ++ fullName ++ ". env: \n\t" ++ show env
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
inferHelper env (IpeFunction args (IpeFunctionBody attributions fnReturn)) = do
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
      attributions

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
run expression = runMonad (infer Map.empty expression)

runWith :: Map.Map String Type -> Expression -> Either String Type
runWith initialVars expression = runMonad (infer env expression)
  where
    env = Map.mapWithKey (\k -> Scheme [k]) initialVars
