module Ipe.Grammar
  ( ModuleDefinition (..),
    ImportList,
    ImportExpression (..),
    TypeDefinition (..),
    CustomTypeConstructor (..),
    IpeType (..),
    Expression (..),
    IpeBinaryOperator (..),
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)

-- | A module definition, with an export list
data ModuleDefinition = ModuleDefinition
  { moduleDefinitionName :: Text,
    exportedDefinitions :: [Text],
    moduleDocComment :: Maybe Text
  }
  deriving (Eq, Show)

-- | A collection of import expressions
type ImportList = [ImportExpression]

-- | An import expression, that may have an alias
data ImportExpression = ImportExpression
  { importedModule :: Text,
    importedModuleAlias :: Maybe Text
  }
  deriving (Eq, Show)

-- | A type definition, which can be an alias, a union or an opaque type.
-- Every type has a name, and may have arguments and a doc comment.
--
-- - Type aliases are an alias to an IpeType
-- - Type unions are a collection of CustomTypeConstructors
-- - Opaque types  are a collection of CustomTypeConstructors
data TypeDefinition
  = TypeAliasDefinition
      { typeAliasDefinitionName :: Text,
        typeAliasDefinitionParameters :: [Text],
        typeAliasDefinitionDocComment :: Maybe Text,
        typeAliasType :: IpeType
      }
  | TypeUnionDefinition
      { typeUnionDefinitionName :: Text,
        typeUnionDefinitionParameters :: [Text],
        typeUnionDefinitionDocComment :: Maybe Text,
        typeUnionDefinitionConstructors :: [CustomTypeConstructor]
      }
  | TypeOpaqueDefinition
      { typeOpaqueDefinitionName :: Text,
        typeOpaqueDefinitionParameters :: [Text],
        typeOpaqueDefinitionDocComment :: Maybe Text,
        typeOpaqueDefinitionConstructors :: [CustomTypeConstructor]
      }
  deriving (Eq, Show)

-- | A custom type constructor used in union types and opaque types.
-- Each constructor has a Name, and may have a doc comment and a list of args.
data CustomTypeConstructor = CustomTypeConstructor
  { customTypeConstructorName :: Text,
    customTypeConstructorDocComment :: Maybe Text,
    customTypeConstructorArgs :: [IpeType]
  }
  deriving (Eq, Show)

-- | A custom type that represents types in type parameters definition
-- constructors. They can be:
-- - ParameterType: starts with a lowercase, and represents an argument in the type definition
-- - ConcreteType: starts with an uppercase, and represents a concrete type, with optional parameters
-- - RecordType: a record, with a dictionary of keys and values that are IpeType
data IpeType
  = ParameterType Text
  | ConcreteType Text [IpeType]
  | RecordType (Map Text IpeType)
  deriving (Eq, Show)

data Expression
  = IpeBinaryOperation IpeBinaryOperator Expression Expression
  | IpeNumber Float
  | IpeString Text
  | -- TODO - Should this be something like { importedFrom :: Text, name :: Text }?
    IpeFunctionCallOrValue Text [Expression]
  deriving (Eq, Show)

data IpeBinaryOperator
  = Add
  | Subtract
  | Divide
  | Multiply
  | Exponentiation
  | PipeRight
  | PipeLeft
  deriving (Eq, Show)
