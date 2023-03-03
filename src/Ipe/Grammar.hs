module Ipe.Grammar
  ( ModuleDefinition (..),
    TopLevelDefinition (..),
    IpeFunctionBody (..),
    ImportList,
    ImportExpression (..),
    TypeDefinition (..),
    CustomTypeConstructor (..),
    IpeType (..),
    TypeAnnotation (..),
    Expression (..),
    IpeMatchPattern (..),
    IpeBinaryOperator (..),
    TypeAlias (..),
    TypeUnion (..),
    TypeOpaque (..),
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
  = TypeAliasDefinition TypeAlias
  | TypeUnionDefinition TypeUnion
  | TypeOpaqueDefinition TypeOpaque
  deriving (Eq, Show)

data TypeAlias = TypeAlias
  { typeAliasDefinitionName :: Text,
    typeAliasDefinitionParameters :: [Text],
    typeAliasDefinitionDocComment :: Maybe Text,
    typeAliasType :: IpeType
  }
  deriving (Eq, Show)

data TypeUnion = TypeUnion
  { typeUnionDefinitionName :: Text,
    typeUnionDefinitionParameters :: [Text],
    typeUnionDefinitionDocComment :: Maybe Text,
    typeUnionDefinitionConstructors :: [CustomTypeConstructor]
  }
  deriving (Eq, Show)

data TypeOpaque = TypeOpaque
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

-- | A function body, which allows any amount of attributions, like
-- `x = 1 + 1`, and a return expression
data IpeFunctionBody = IpeFunctionBody
  { attributions :: [(Text, Expression)],
    functionReturn :: Expression
  }
  deriving (Eq, Show)

-- | A top level definition, which can be an expression or a function, with a name, and optional doc comment and type annotation
data TopLevelDefinition = TopLevelDefinition
  { topLevelDefinitionName :: Text,
    topLevelDefinitionDocComment :: Maybe Text,
    topLevelDefinitionValue :: Expression,
    topLevelDefinitionTypeAnnotation :: Maybe TypeAnnotation
  }
  deriving (Eq, Show)

data TypeAnnotation = TypeAnnotation
  { typeAnnotationName :: Text,
    typeAnnotationArguments :: [IpeType],
    typeAnnotationReturnType :: IpeType
  }
  deriving (Eq, Show)

-- | An expression, which can be:
-- - A binary operation, with an operator and two expressions
-- - A number
-- - A string
-- - A function call or value, with a name and a list of arguments
-- - A match expression
-- - A function definition, with a list of arguments and a function definition body
data Expression
  = IpeBinaryOperation IpeBinaryOperator Expression Expression
  | IpeNumber Float
  | IpeMatch Expression [(IpeMatchPattern, Expression)]
  | IpeString Text
  | -- TODO - Support record accessors
    IpeFunctionCallOrValue [Text] Text [Expression]
  | IpeFunction [Text] IpeFunctionBody
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

data IpeMatchPattern
  = IpeWildCardPattern
  | IpeVariablePattern Text
  | IpeCustomTypePattern Text [IpeMatchPattern]
  | IpeLiteralNumberPattern Float
  | IpeLiteralStringPattern Text
  deriving (Eq, Show)
