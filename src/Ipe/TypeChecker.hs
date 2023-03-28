{-# LANGUAGE LambdaCase #-}

module Ipe.TypeChecker
  ( Type (..),
    TypeInferenceMonad,
    TypeEnv (..),
    Scheme (..),
    Substitution,
    runMonad,
    customTypeFromConstructorName,
    mostGeneralUnifier,
    compose,
    apply,
    remove,
    generalize,
    newTypeVar,
    instantiate,
    freeTypeVariables,
  )
where

-- \| This module uses Algorithm W, explained in https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf

import qualified Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get, put)
import qualified Data.Bifunctor
import qualified Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

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
  | TCustom String [Type] [(String, [Type])] -- name + type variables + constructors
  deriving (Eq)

instance Show Type where
  show (TVar name) = name
  show TNum = "Number"
  show TStr = "String"
  show (TFun input output) = "(" ++ show input ++ " -> " ++ show output ++ ")"
  show (TRec []) = "{}"
  show (TRec fields) = "{ " ++ Data.List.intercalate ", " (map (\(name, t) -> name ++ ": " ++ show t) fields) ++ " }"
  show (TCustom name [] _) = name
  show (TCustom name typeVars _) = "(" ++ name ++ (" " ++ unwords (map show typeVars)) ++ ")"

instance Types Type where
  freeTypeVariables (TVar var) = Set.singleton var
  freeTypeVariables TNum = Set.empty
  freeTypeVariables TStr = Set.empty
  freeTypeVariables (TFun input output) = Set.union (freeTypeVariables input) (freeTypeVariables output)
  freeTypeVariables (TRec fields) = Set.unions (map (freeTypeVariables . snd) fields)
  freeTypeVariables (TCustom _ typeVars _) = Set.unions (map freeTypeVariables typeVars)

  apply subs (TVar var) = case Map.lookup var subs of
    Nothing -> TVar var
    Just t -> t
  apply subs (TFun input output) = TFun (apply subs input) (apply subs output)
  apply _ TNum = TNum
  apply _ TStr = TStr
  apply subs (TRec fields) = TRec (map (Data.Bifunctor.second (apply subs)) fields)
  apply subs (TCustom name typeVars constructors) = TCustom name (map (apply subs) typeVars) constructors

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
mostGeneralUnifier (TCustom name1 typeVars1 _) (TCustom name2 typeVars2 _)
  | name1 /= name2 = throwE $ "can't match expected type\n\t" ++ name1 ++ "\nwith actual type\n\t" ++ name2
  | length typeVars1 /= length typeVars2 = throwE $ "can't match expected type\n\t" ++ name1 ++ "\nwith actual type\n\t" ++ name2 ++ "\n(number of type variables does not match)"
  | otherwise =
      Control.Monad.foldM
        (\acc (t1, t2) -> mostGeneralUnifier (apply acc t1) (apply acc t2))
        Map.empty
        $ zip typeVars1 typeVars2
mostGeneralUnifier t1 t2 = throwE $ "can't match expected type\n\t" ++ show t1 ++ "\nwith actual type\n\t" ++ show t2

varBind :: String -> Type -> TypeInferenceMonad Substitution
varBind var t
  | t == TVar var = return Map.empty
  | var `Set.member` freeTypeVariables t = throwE $ "occurs check fails: " ++ var ++ " in " ++ show t
  | otherwise = return $ Map.singleton var t

customTypeFromConstructorName :: String -> TypeEnv -> Maybe (String, Type, [(String, [Type])], [Type])
customTypeFromConstructorName constructorName (TypeEnv env) =
  findMap
    ( \case
        t@(TCustom name _ constructors) ->
          (\(_, ts) -> (name, t, constructors, ts))
            <$> Data.List.find
              (\(constructor, _) -> constructor == constructorName)
              constructors
        _ -> Nothing
    )
    $ map (\(Scheme _ t) -> t)
    $ Map.elems env

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap _ [] = Nothing
findMap f (x : xs) =
  case f x of
    Just y -> Just y
    Nothing -> findMap f xs

-- MAIN FUNCTIONS

runMonad :: TypeInferenceMonad a -> Either String a
runMonad t =
  evalState (runExceptT t) initialInferenceState

initialInferenceState :: InferenceState
initialInferenceState = InferenceState {lastVarIndex = 0, currentSubstitution = Map.empty}
