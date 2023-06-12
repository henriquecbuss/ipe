{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.Module (run, Error (..)) where

import Control.Monad (mapAndUnzipM)
import qualified Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get, modify, put)
import qualified Data.Bifunctor
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Ipe.Grammar
  ( CustomTypeConstructor (..),
    ImportExpression (..),
    IpeType (..),
    Module (..),
    ModuleDefinition (..),
    TopLevelDefinition (..),
    TypeAlias (..),
    TypeAnnotation (..),
    TypeDefinition (..),
    TypeOpaque (..),
    TypeUnion (..),
  )
import qualified Ipe.Prelude.Prelude
import qualified Ipe.TypeChecker.Expression
import Ipe.TypeChecker.Utils (Type (..), freeTypeVariables)
import qualified Ipe.TypeChecker.Utils as Ipe.TypeChecker

run ::
  Map.Map ([Text], Text) (Module, Map.Map Text Type) ->
  Module ->
  Either Error (Map.Map ([Text], Text) (Module, Map.Map Text Type))
run allModules currModule =
  evalState (runExceptT (runHelper allModules currModule)) (defaultInitialState currModule)

runHelper ::
  Map.Map ([Text], Text) (Module, Map.Map Text Type) ->
  Module ->
  CollectionMonad (Map.Map ([Text], Text) (Module, Map.Map Text Type))
runHelper allModules currModule@(Module {moduleImports, typeDefinitions, topLevelDefinitions}) = do
  let allImportedModules = moduleImports

  contextWithImports <-
    Control.Monad.foldM
      ( \acc importExpr ->
          if T.intercalate "." (importedModulePath importExpr ++ [importedModule importExpr]) `elem` Ipe.Prelude.Prelude.allModuleNames
            then return acc
            else case Map.lookup (importedModulePath importExpr, importedModule importExpr) allModules of
              Nothing -> throwE $ ModuleDoesNotExist (importedModulePath importExpr, importedModule importExpr)
              Just (importedModule, importedMap) ->
                if not (null importedMap)
                  then return acc
                  else do except $ run allModules importedModule
      )
      allModules
      allImportedModules

  initState <- lift get

  let importAliases =
        Map.fromList $
          List.map
            ( \(ImportExpression {importedModulePath, importedModule, importedModuleAlias}) ->
                ( ( importedModulePath,
                    importedModule
                  ),
                  (\(p, m) -> T.intercalate "." (p <> [m])) $
                    Maybe.fromMaybe
                      (importedModulePath, importedModule)
                      importedModuleAlias
                )
            )
            moduleImports

  let importsOnThisModule =
        Map.foldlWithKey
          ( \currentState pathAndName (_, types) ->
              case Map.lookup pathAndName importAliases of
                Nothing -> currentState
                Just validAlias -> Map.union currentState $ Map.mapKeys (\k -> validAlias <> "." <> k) types
          )
          initState
          contextWithImports

  lift $ put importsOnThisModule

  mapM_ (addTypeDefinition currModule) typeDefinitions

  mapM_ (addTopLevelDefinition currModule) topLevelDefinitions

  stateAfterAddingEverything <- lift get
  let allExportedDefinitions = exportedDefinitions $ moduleDefinition currModule
  let exportedCustomTypeConstructors =
        concat $
          Maybe.mapMaybe
            ( \case
                TypeUnionDefinition (TypeUnion {typeUnionDefinitionName, typeUnionDefinitionConstructors}) ->
                  if typeUnionDefinitionName `elem` allExportedDefinitions
                    then Just $ map customTypeConstructorName typeUnionDefinitionConstructors
                    else Nothing
                TypeAliasDefinition _ -> Nothing
                TypeOpaqueDefinition _ -> Nothing
            )
            typeDefinitions

  let allDefinitionsThatShouldBeExported = allExportedDefinitions ++ exportedCustomTypeConstructors

  exportedTypes <-
    if allDefinitionsThatShouldBeExported `List.intersect` Map.keys stateAfterAddingEverything == allDefinitionsThatShouldBeExported
      then do
        return $ Map.filterWithKey (\k _ -> k `elem` allDefinitionsThatShouldBeExported) stateAfterAddingEverything
      else throwE $ NotAllVariablesDeclared allExportedDefinitions (Map.keys stateAfterAddingEverything)

  return $
    Map.insert
      ( moduleDefinitionPath $ moduleDefinition currModule,
        moduleDefinitionName $ moduleDefinition currModule
      )
      (currModule, exportedTypes)
      contextWithImports

defaultInitialState :: Module -> Map.Map Text Type
defaultInitialState currModule =
  Map.union
    ( Map.fromList
        [ ("Number", TNum),
          ("String", TStr),
          ("List", TList (TVar "a")),
          ("Result", TCustom "Result" [TVar "err", TVar "ok"] [("Err", [TVar "err"]), ("Ok", [TVar "ok"])]),
          ("Maybe", TCustom "Maybe" [TVar "a"] [("Nothing", []), ("Just", [TVar "a"])])
        ]
    )
    ( Map.unions . Map.elems $
        Map.filterWithKey
          (\k _ -> k `elem` importedPaths)
          Ipe.Prelude.Prelude.registerAllModules
    )
  where
    importedPaths =
      map
        ( \importExpr ->
            T.intercalate "." (importedModulePath importExpr <> [importedModule importExpr])
        )
        (moduleImports currModule)

data Error
  = NotAllVariablesDeclared [Text] [Text] -- required + given
  | UnknownType Text
  | TopLevelDefinitionError Text Ipe.TypeChecker.Error
  | AnnotationNameMismatch Text Text -- expected + actual
  | ModuleDoesNotExist ([Text], Text)
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
  show (TopLevelDefinitionError tldName err) =
    "Error in top level definition " <> show tldName <> ":\n\n\t" <> show err
  show (AnnotationNameMismatch expected actual) =
    "The type annotation for this definition doesn't match the name of the definition! Expected: " <> show expected <> ", but got: " <> show actual
  show (ModuleDoesNotExist (path, name)) =
    "The module " <> show (T.intercalate "." (path <> [name])) <> " does not exist!"

type CollectionMonad a = ExceptT Error (State (Map.Map Text Type)) a

addToCollection :: Text -> Type -> CollectionMonad Type
addToCollection name type_ =
  lift $ modify (Map.insert name type_) >> return type_

getFromCollection :: Text -> CollectionMonad (Maybe Type)
getFromCollection name =
  lift $ Map.lookup name <$> get

applyArg :: Module -> IpeType -> Text -> Type -> CollectionMonad Type
applyArg currModule argType argName (TVar varName) =
  if T.pack varName == argName
    then snd <$> ipeTypeToType currModule argType
    else return (TVar varName)
applyArg currModule argType argName (TFun input output) = do
  inputReplaced <- applyArg currModule argType argName input
  outputReplaced <- applyArg currModule argType argName output

  return $ TFun inputReplaced outputReplaced
applyArg currModule argType argName (TRec fields) = do
  TRec
    <$> mapM
      ( \(fieldName, fieldType) -> do
          replacedFieldType <- applyArg currModule argType argName fieldType
          return (fieldName, replacedFieldType)
      )
      fields
applyArg currModule argType argName (TCustom name typeVars constructors) = do
  replacedTypeVars <- mapM (applyArg currModule argType argName) typeVars

  replacedConstructors <-
    mapM
      ( \(constructorName, constructorParams) -> do
          replacedParams <- mapM (applyArg currModule argType argName) constructorParams
          return (constructorName, replacedParams)
      )
      constructors

  return $ TCustom name replacedTypeVars replacedConstructors
applyArg _ _ _ t = return t

applyArgs :: Module -> [IpeType] -> [Text] -> Type -> CollectionMonad Type
applyArgs _ [] _ t = return t
applyArgs _ _ [] t = return t
applyArgs currModule [arg] [argName] t = applyArg currModule arg argName t
applyArgs currModule (arg : args) (argName : argNames) t =
  applyArg currModule arg argName t
    >>= applyArgs currModule args argNames

addTypeDefinition :: Module -> TypeDefinition -> CollectionMonad Type
addTypeDefinition currModule t =
  case t of
    TypeAliasDefinition (TypeAlias {typeAliasDefinitionName, typeAliasDefinitionParameters, typeAliasType}) ->
      do
        (requiredArgs, type_) <-
          ipeTypeToType
            currModule
            typeAliasType

        applyArgs
          currModule
          (List.map ParameterType typeAliasDefinitionParameters)
          (Set.toList requiredArgs)
          type_
          >>= checkTypeParams requiredArgs (Set.fromList typeAliasDefinitionParameters)
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
      >>= applyArgs
        currModule
        (List.map ParameterType typeParams)
        (Set.toList requiredArgs)

  mapM_
    ( \(constructorName, constructorArgs) ->
        addToCollection (T.pack constructorName) (List.foldr TFun typeDef constructorArgs)
          >>= applyArgs
            currModule
            (List.map ParameterType typeParams)
            (Set.toList requiredArgs)
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

      typeDef <- case maybeExistingType of
        Nothing ->
          case findTypeDefinitionByName currModule fullName of
            Nothing -> throwE $ UnknownType fullName
            Just typeDefinition ->
              addTypeDefinition currModule typeDefinition
        Just existingType ->
          return existingType

      appliedTypeDef <-
        applyArgs
          currModule
          args
          (List.map T.pack $ Set.toList $ freeTypeVariables typeDef)
          typeDef

      return (requiredArgs, appliedTypeDef)
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

addTopLevelDefinition :: Module -> TopLevelDefinition -> CollectionMonad Type
addTopLevelDefinition
  currModule
  originalTld@( TopLevelDefinition
                  { topLevelDefinitionName,
                    topLevelDefinitionValue,
                    topLevelDefinitionTypeAnnotation
                  }
                ) = do
    existingType <- getFromCollection topLevelDefinitionName

    case existingType of
      Just x -> return x
      Nothing -> do
        availableTypes <- Map.mapKeys T.unpack <$> lift get

        inferredType <- case Ipe.TypeChecker.Expression.runWith availableTypes topLevelDefinitionValue of
          Left err@(Ipe.TypeChecker.UnboundVariable unboundVarName) ->
            case findTopLevelDefinition currModule unboundVarName of
              Nothing -> throwE $ TopLevelDefinitionError topLevelDefinitionName err
              Just newTld -> do
                _ <- addTopLevelDefinition currModule newTld
                addTopLevelDefinition currModule originalTld
          Left err -> throwE $ TopLevelDefinitionError topLevelDefinitionName err
          Right t -> return t

        case topLevelDefinitionTypeAnnotation of
          Nothing -> addToCollection topLevelDefinitionName inferredType
          Just annotation@(TypeAnnotation {typeAnnotationName}) ->
            if typeAnnotationName /= topLevelDefinitionName
              then throwE $ AnnotationNameMismatch topLevelDefinitionName typeAnnotationName
              else do
                annotationType <- typeAnnotationToType currModule annotation

                let unifiedType = Ipe.TypeChecker.runMonad (Ipe.TypeChecker.mostGeneralUnifier inferredType annotationType)

                case unifiedType of
                  Left err -> throwE $ TopLevelDefinitionError topLevelDefinitionName err
                  Right substitution -> do
                    addToCollection topLevelDefinitionName (Ipe.TypeChecker.apply substitution annotationType)

findTopLevelDefinition :: Module -> String -> Maybe TopLevelDefinition
findTopLevelDefinition (Module {topLevelDefinitions}) tldName =
  List.find
    ( \(TopLevelDefinition {topLevelDefinitionName}) ->
        T.pack tldName == topLevelDefinitionName
    )
    topLevelDefinitions

typeAnnotationToType :: Module -> TypeAnnotation -> CollectionMonad Type
typeAnnotationToType currModule (TypeAnnotation {typeAnnotationArguments, typeAnnotationReturnType})
  | null typeAnnotationArguments = ipeTypeToType currModule typeAnnotationReturnType <&> snd
  | otherwise = do
      (_, returnType) <- ipeTypeToType currModule typeAnnotationReturnType

      List.foldr
        ( \arg acc -> do
            (_, argType) <- ipeTypeToType currModule arg

            TFun argType <$> acc
        )
        (return returnType)
        typeAnnotationArguments
