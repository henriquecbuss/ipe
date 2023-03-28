{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.Module (run, Error (..)) where

import Control.Monad (mapAndUnzipM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, modify, runState)
import qualified Data.Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Grammar (CustomTypeConstructor (..), IpeType (..), Module (..), TypeAlias (..), TypeDefinition (..), TypeOpaque (..), TypeUnion (..))
import Ipe.TypeChecker (Type (..))

run :: Module -> Either Error (Map.Map Text Type)
run currModule =
  case runState (runExceptT (runHelper currModule)) initialState of
    (Left err, _) -> Left err
    (Right _, s) -> Right s

initialState :: Map.Map Text Type
initialState =
  Map.fromList
    [ ("Number", TNum),
      ("String", TStr)
    ]

runHelper :: Module -> CollectionMonad ()
runHelper currModule@(Module {typeDefinitions}) = do
  mapM_ (addTypeDefinition currModule) typeDefinitions

data Error
  = NotAllVariablesDeclared [Text] [Text] -- required + given
  | UnknownType Text
  deriving (Eq)

instance Show Error where
  show (NotAllVariablesDeclared required given) =
    T.unpack $
      T.concat
        [ "There's something wrong with your type arguments! The type requires these arguments:\n\n\t",
          T.intercalate ", " required,
          "\n\nBut you gave it these:\n\n\t",
          T.intercalate ", " given
        ]
  show (UnknownType typeName) =
    "Unknown type: " ++ T.unpack typeName

type CollectionMonad a = ExceptT Error (State (Map.Map Text Type)) a

addToCollection :: Text -> Type -> CollectionMonad Type
addToCollection name type_ =
  lift $ modify (Map.insert name type_) >> return type_

getFromCollection :: Text -> CollectionMonad (Maybe Type)
getFromCollection name =
  lift $ Map.lookup name <$> get

addTypeDefinition :: Module -> TypeDefinition -> CollectionMonad Type
addTypeDefinition currModule t =
  case t of
    TypeAliasDefinition (TypeAlias {typeAliasDefinitionName, typeAliasDefinitionParameters, typeAliasType}) ->
      do
        (requiredArgs, type_) <- ipeTypeToType currModule typeAliasType

        checkTypeParams requiredArgs (Set.fromList typeAliasDefinitionParameters) type_
          >>= addToCollection typeAliasDefinitionName
    TypeUnionDefinition (TypeUnion {typeUnionDefinitionName, typeUnionDefinitionParameters, typeUnionDefinitionConstructors}) ->
      addCustomType currModule typeUnionDefinitionName typeUnionDefinitionParameters typeUnionDefinitionConstructors
    TypeOpaqueDefinition (TypeOpaque {typeOpaqueDefinitionName, typeOpaqueDefinitionParameters, typeOpaqueDefinitionConstructors}) ->
      addCustomType currModule typeOpaqueDefinitionName typeOpaqueDefinitionParameters typeOpaqueDefinitionConstructors

addCustomType :: Module -> Text -> [Text] -> [CustomTypeConstructor] -> CollectionMonad Type
addCustomType currModule name typeParams constructors = do
  (requiredArgs, constructorTypes) <-
    Data.Bifunctor.first Set.unions
      <$> mapAndUnzipM
        ( \(CustomTypeConstructor {customTypeConstructorName, customTypeConstructorArgs}) -> do
            (setsOfParameters, args) <- mapAndUnzipM (ipeTypeToType currModule) customTypeConstructorArgs
            let requiredParameters = Set.unions setsOfParameters
            return (requiredParameters, (T.unpack customTypeConstructorName, args))
        )
        constructors

  typeDef <-
    addToCollection
      name
      ( TCustom
          (T.unpack name)
          (List.map (TVar . T.unpack) typeParams)
          constructorTypes
      )

  mapM_
    ( \(constructorName, constructorArgs) ->
        addToCollection (T.pack constructorName) $
          List.foldr TFun typeDef constructorArgs
    )
    constructorTypes

  checkTypeParams requiredArgs (Set.fromList typeParams) typeDef

checkTypeParams :: Set Text -> Set Text -> Type -> CollectionMonad Type
checkTypeParams required provided returnType =
  if Set.intersection required provided == required
    then return returnType
    else throwE $ NotAllVariablesDeclared (Set.toList required) (Set.toList provided)

ipeTypeToType :: Module -> IpeType -> CollectionMonad (Set Text, Type)
ipeTypeToType currModule t =
  case t of
    ParameterType paramName -> return (Set.fromList [paramName], TVar $ T.unpack paramName)
    ConcreteType path name args -> do
      -- TODO - Apply args
      let fullName = T.intercalate "." (path ++ [name])
      maybeExistingType <- getFromCollection fullName

      let requiredArgs =
            Set.fromList $
              Maybe.mapMaybe
                ( \case
                    ParameterType p -> Just p
                    _ -> Nothing
                )
                args

      case maybeExistingType of
        Nothing ->
          case findTypeDefinitionByName currModule fullName of
            Nothing -> throwE $ UnknownType fullName
            Just typeDefinition -> do
              x <- addTypeDefinition currModule typeDefinition
              return (requiredArgs, x)
        Just existingType ->
          return (requiredArgs, existingType)
    RecordType r -> do
      Data.Bifunctor.bimap Set.unions TRec . List.unzip
        <$> mapM
          ( \(fieldName, fieldType) -> do
              (requiredArgs, result) <- ipeTypeToType currModule fieldType
              return (requiredArgs, (T.unpack fieldName, result))
          )
          (Map.toList r)

findTypeDefinitionByName :: Module -> Text -> Maybe TypeDefinition
findTypeDefinitionByName currModule name =
  List.find
    (\t -> nameFromTypeDefinition t == name)
    (typeDefinitions currModule)

nameFromTypeDefinition :: TypeDefinition -> Text
nameFromTypeDefinition t =
  case t of
    TypeAliasDefinition (TypeAlias {typeAliasDefinitionName}) ->
      typeAliasDefinitionName
    TypeUnionDefinition (TypeUnion {typeUnionDefinitionName}) ->
      typeUnionDefinitionName
    TypeOpaqueDefinition (TypeOpaque {typeOpaqueDefinitionName}) ->
      typeOpaqueDefinitionName
