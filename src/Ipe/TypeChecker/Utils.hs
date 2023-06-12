{-# LANGUAGE LambdaCase #-}

module Ipe.TypeChecker.Utils
  ( Type (..),
    TypeInferenceMonad,
    TypeEnv (..),
    Scheme (..),
    Error (..),
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
    prefix,
  )
where

-- \| This module uses Algorithm W, explained in https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf

import qualified Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get, put)
import qualified Data.Bifunctor
import Data.Char as Char
import qualified Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Ipe.Grammar

-- TYPES

class Types a where
  freeTypeVariables :: a -> Set.Set String
  apply :: Substitution -> a -> a

instance (Types a) => Types [a] where
  freeTypeVariables = foldr (Set.union . freeTypeVariables) Set.empty
  apply = map . apply

data Type
  = TVar String
  | TNum
  | TStr
  | TFun Type Type
  | TRec [(String, Type)]
  | TList Type
  | TCustom String [Type] [(String, [Type])] -- name + type variables + constructors
  deriving (Eq)

instance Show Type where
  show (TVar name) = name
  show TNum = "Number"
  show TStr = "String"
  show (TFun input output) = "(" ++ show input ++ " -> " ++ show output ++ ")"
  show (TRec []) = "{}"
  show (TRec fields) = "{ " ++ Data.List.intercalate ", " (map (\(name, t) -> name ++ ": " ++ show t) fields) ++ " }"
  show (TList t) = "List " <> show t
  show (TCustom name [] _) = name
  show (TCustom name typeVars _) = "(" ++ name ++ (" " ++ unwords (map show typeVars)) ++ ")"

instance Types Type where
  freeTypeVariables (TVar var) = Set.singleton var
  freeTypeVariables TNum = Set.empty
  freeTypeVariables TStr = Set.empty
  freeTypeVariables (TFun input output) = Set.union (freeTypeVariables input) (freeTypeVariables output)
  freeTypeVariables (TRec fields) = Set.unions (map (freeTypeVariables . snd) fields)
  freeTypeVariables (TList inner) = freeTypeVariables inner
  freeTypeVariables (TCustom _ typeVars _) = Set.unions (map freeTypeVariables typeVars)

  apply subs (TVar var) = case Map.lookup var subs of
    Nothing -> TVar var
    Just t -> t
  apply subs (TFun input output) = TFun (apply subs input) (apply subs output)
  apply _ TNum = TNum
  apply _ TStr = TStr
  apply subs (TRec fields) = TRec (map (Data.Bifunctor.second (apply subs)) fields)
  apply subs (TList inner) = TList (apply subs inner)
  apply subs (TCustom name typeVars constructors) = TCustom name (map (apply subs) typeVars) constructors

data Scheme = Scheme [String] Type deriving (Show)

instance Types Scheme where
  freeTypeVariables (Scheme vars t) = Set.difference (freeTypeVariables t) (Set.fromList vars)
  apply subs (Scheme vars t) = Scheme vars (apply (foldr Map.delete subs vars) t)

type Substitution = Map.Map String Type

newtype TypeEnv = TypeEnv (Map.Map String Scheme) deriving (Show)

instance Types TypeEnv where
  freeTypeVariables (TypeEnv env) = freeTypeVariables (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

data InferenceState = InferenceState {lastVarIndex :: Int, currentSubstitution :: Substitution}

type TypeInferenceMonad a = ExceptT Error (State InferenceState) a

data Error
  = NoMatch Type Type -- expected + actual
  | PossiblyInfiniteType String Type
  | InvalidOperation Ipe.Grammar.IpeBinaryOperator Type Type
  | PatternMatchOnFunction
  | PatternMatchOnRecord
  | MissingPatternMatchCases
  | PatternMatchOnHandledPatternMatch
  | InvalidTypeForPatternMatch
  | DuplicatePatternMatch
  | ConstructorNotFound String
  | InvalidNumberOfArguments Int Int
  | UnboundVariable String
  | MissingRecordField [(String, Type)] String
  | TooManyArguments Type Type
  | ModuleDoesNotExist ([Text], Text)
  | NoArguments
  deriving (Show, Eq)

-- HELPER FUNCTIONS

prefix :: [Text] -> Text -> Type -> Type
prefix modulePath moduleName t =
  case t of
    TNum -> TNum
    TStr -> TStr
    TVar var -> TVar var
    TFun input output -> TFun (prefix modulePath moduleName input) (prefix modulePath moduleName output)
    TRec record -> TRec $ map (Data.Bifunctor.second (prefix modulePath moduleName)) record
    TList inner -> TList $ prefix modulePath moduleName inner
    TCustom name typeVars constructors ->
      TCustom
        (prefixNonImported name)
        (map (prefix modulePath moduleName) typeVars)
        ( map
            ( Data.Bifunctor.bimap
                prefixNonImported
                (map (prefix modulePath moduleName))
            )
            constructors
        )
  where
    prefixString :: String -> String
    prefixString str = Data.List.intercalate "." (map T.unpack modulePath ++ [T.unpack moduleName, str])

    prefixNonImported :: String -> String
    prefixNonImported str =
      if isImported str then str else prefixString str

    isImported :: String -> Bool
    isImported [] = False
    isImported (first : rest)
      | not (Char.isUpper first) = False
      | otherwise = '.' `elem` rest

compose :: Substitution -> Substitution -> Substitution
compose subs1 subs2 = Map.map (apply subs1) subs2 `Map.union` subs1

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where
    vars = Set.toList $ Set.difference (freeTypeVariables t) (freeTypeVariables env)

newTypeVar :: String -> TypeInferenceMonad Type
newTypeVar namePrefix = do
  currentState <- lift get
  lift $ put $ currentState {lastVarIndex = lastVarIndex currentState + 1}
  return $ TVar (namePrefix ++ show (lastVarIndex currentState))

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
mostGeneralUnifier t1@(TRec fields1) t2@(TRec fields2)
  | length fields1 == length fields2 = do
      let (names1, types1) = unzip fields1
      let (names2, types2) = unzip fields2
      if names1 == names2
        then do
          subs <- Control.Monad.zipWithM mostGeneralUnifier types1 types2
          return $ foldr compose Map.empty subs
        else throwE $ NoMatch t1 t2
  | otherwise = throwE $ NoMatch t1 t2
mostGeneralUnifier (TList t1) (TList t2) = mostGeneralUnifier t1 t2
mostGeneralUnifier t1@(TCustom name1 typeVars1 _) t2@(TCustom name2 typeVars2 _)
  | name1 /= name2 = throwE $ NoMatch t1 t2
  | length typeVars1 /= length typeVars2 = throwE $ NoMatch t1 t2
  | otherwise =
      Control.Monad.foldM
        (\acc (first, second) -> mostGeneralUnifier (apply acc first) (apply acc second))
        Map.empty
        $ zip typeVars1 typeVars2
mostGeneralUnifier t1 t2 = throwE $ NoMatch t1 t2

varBind :: String -> Type -> TypeInferenceMonad Substitution
varBind var t
  | t == TVar var = return Map.empty
  | var `Set.member` freeTypeVariables t = throwE $ PossiblyInfiniteType var t
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

runMonad :: TypeInferenceMonad a -> Either Error a
runMonad t =
  evalState (runExceptT t) initialInferenceState

initialInferenceState :: InferenceState
initialInferenceState = InferenceState {lastVarIndex = 0, currentSubstitution = Map.empty}
