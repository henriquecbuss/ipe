{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.Module (run) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, modify, runState)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Grammar (CustomTypeConstructor (..), IpeType (..), Module (..), TypeAlias (..), TypeDefinition (..), TypeOpaque (..), TypeUnion (..))
import Ipe.TypeChecker (Type (..))

-- TODO - Test
run :: Module -> Either Error (Map.Map Text Type)
run currModule =
  case runState (runExceptT (runHelper currModule)) Map.empty of
    (Left err, _) -> Left err
    (Right _, s) -> Right s

runHelper :: Module -> CollectionMonad ()
runHelper currModule@(Module {typeDefinitions}) = do
  mapM_ (addTypeDefinition currModule) typeDefinitions

  undefined

data Error
  = NotAllVariablesDeclared [String] [String] -- declared + non-declared
  | UnknownType Text

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
    TypeAliasDefinition (TypeAlias {typeAliasDefinitionName, typeAliasType}) ->
      -- TODO - Make sure all args are defined
      ipeTypeToType currModule typeAliasType
        >>= addToCollection typeAliasDefinitionName
    TypeUnionDefinition (TypeUnion {typeUnionDefinitionName, typeUnionDefinitionParameters, typeUnionDefinitionConstructors}) ->
      -- TODO - Make sure all args are defined
      addCustomType currModule typeUnionDefinitionName typeUnionDefinitionParameters typeUnionDefinitionConstructors
    TypeOpaqueDefinition (TypeOpaque {typeOpaqueDefinitionName, typeOpaqueDefinitionParameters, typeOpaqueDefinitionConstructors}) ->
      -- TODO - Make sure all args are defined
      addCustomType currModule typeOpaqueDefinitionName typeOpaqueDefinitionParameters typeOpaqueDefinitionConstructors

addCustomType :: Module -> Text -> [Text] -> [CustomTypeConstructor] -> CollectionMonad Type
addCustomType currModule name typeParams constructors = do
  constructorTypes <-
    mapM
      ( \(CustomTypeConstructor {customTypeConstructorName, customTypeConstructorArgs}) -> do
          args <- mapM (ipeTypeToType currModule) customTypeConstructorArgs
          return (T.unpack customTypeConstructorName, args)
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

  return typeDef

ipeTypeToType :: Module -> IpeType -> CollectionMonad Type
ipeTypeToType currModule t =
  case t of
    ParameterType paramName -> return $ TVar $ T.unpack paramName
    ConcreteType path name args -> do
      -- TODO - Make sure all args are defined
      let fullName = T.intercalate "." (path ++ [name])
      maybeExistingType <- getFromCollection fullName

      case maybeExistingType of
        Nothing ->
          case findTypeDefinitionByName currModule fullName of
            Nothing -> throwE $ UnknownType fullName
            Just typeDefinition -> do
              addTypeDefinition currModule typeDefinition
        Just existingType ->
          return existingType
    RecordType r ->
      TRec
        <$> mapM
          ( \(fieldName, fieldType) -> do
              result <- ipeTypeToType currModule fieldType
              return (T.unpack fieldName, result)
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
