{-# LANGUAGE OverloadedStrings #-}

module Ipe.TypeChecker.ModuleSpec (spec) where

import qualified Data.Map as Map
import Data.Text (Text)
import Ipe.Grammar
import Ipe.TypeChecker (Type (..))
import qualified Ipe.TypeChecker as TypeChecker
import qualified Ipe.TypeChecker.Module as ModTypeChecker
import Test.Hspec

spec :: Spec
spec = describe "the module type checker" $ do
  addingTypeDefinitions

addingTypeDefinitions :: Spec
addingTypeDefinitions =
  describe "when adding type definitions" $ do
    it "should type check alias + union" $
      ModTypeChecker.run sampleModule
        `shouldBe` Right
          ( Map.fromList
              [ ("Number", TNum),
                ("String", TStr),
                ("AliasTest", tCustom),
                ("UnionTest", tCustom),
                ("OpaqueTest", tOpaque),
                ("A", TFun TNum tCustom),
                ("B", TFun TStr (TFun TNum (TFun (TVar "b") tCustom))),
                ("C", TFun tOpaque tCustom),
                ("OA", TFun (TVar "a") tOpaque),
                ("OB", TFun (TVar "b") tOpaque)
              ]
          )

    it "should recognize when a type doesn't exist" $
      ModTypeChecker.run
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" [] $
                    ConcreteType [] "UnionTest" []
                ]
            }
        )
        `shouldBe` Left (ModTypeChecker.UnknownType "UnionTest")

    it "should recognize when a type isn't declaring all variables" $
      ModTypeChecker.run
        ( sampleModule
            { typeDefinitions =
                [ TypeAliasDefinition . typeAlias "AliasTest" ["a"] $
                    ConcreteType [] "UnionTest" [ParameterType "a", ParameterType "b"],
                  TypeUnionDefinition $
                    typeUnion
                      "UnionTest"
                      ["a", "b"]
                      [("A", [ConcreteType [] "Number" []])]
                ]
            }
        )
        `shouldBe` Left (ModTypeChecker.NotAllVariablesDeclared ["a", "b"] ["a"])
  where
    sampleModule :: Module
    sampleModule =
      Module
        { moduleDefinition = sampleModuleDefinition,
          moduleImports = [],
          typeDefinitions = sampleTypeDefinitions0,
          topLevelDefinitions = []
        }

    tCustom :: TypeChecker.Type
    tCustom =
      TCustom
        "UnionTest"
        [TVar "a", TVar "b"]
        [ ("A", [TNum]),
          ("B", [TStr, TNum, TVar "b"]),
          ("C", [tOpaque])
        ]

    tOpaque :: TypeChecker.Type
    tOpaque =
      TCustom
        "OpaqueTest"
        [TVar "a", TVar "b"]
        [ ("OA", [TVar "a"]),
          ("OB", [TVar "b"])
        ]

sampleModuleDefinition :: ModuleDefinition
sampleModuleDefinition =
  ModuleDefinition
    { moduleDefinitionPath = [],
      moduleDefinitionName = "Test",
      exportedDefinitions = [],
      moduleDocComment = Nothing
    }

sampleTypeDefinitions0 :: [TypeDefinition]
sampleTypeDefinitions0 =
  [ TypeAliasDefinition $ typeAlias "AliasTest" [] $ ConcreteType [] "UnionTest" [],
    TypeUnionDefinition typeUnion0,
    TypeOpaqueDefinition typeOpaque0
  ]
  where
    typeUnion0 =
      typeUnion
        "UnionTest"
        ["a", "b"]
        [ ("A", [ConcreteType [] "Number" []]),
          ("B", [ConcreteType [] "String" [], ConcreteType [] "Number" [], ParameterType "b"]),
          ("C", [ConcreteType [] "OpaqueTest" [ParameterType "a", ParameterType "b"]])
        ]

    typeOpaque0 =
      typeOpaque
        "OpaqueTest"
        ["a", "b"]
        [ ("OA", [ParameterType "a"]),
          ("OB", [ParameterType "b"])
        ]

typeAlias :: Text -> [Text] -> IpeType -> TypeAlias
typeAlias name parameters type_ =
  TypeAlias
    { typeAliasDefinitionName = name,
      typeAliasDefinitionParameters = parameters,
      typeAliasDefinitionDocComment = Nothing,
      typeAliasType = type_
    }

typeUnion :: Text -> [Text] -> [(Text, [IpeType])] -> TypeUnion
typeUnion name parameters constructors =
  TypeUnion
    { typeUnionDefinitionName = name,
      typeUnionDefinitionParameters = parameters,
      typeUnionDefinitionDocComment = Nothing,
      typeUnionDefinitionConstructors = map customTypeConstructor constructors
    }

typeOpaque :: Text -> [Text] -> [(Text, [IpeType])] -> TypeOpaque
typeOpaque name parameters constructors =
  TypeOpaque
    { typeOpaqueDefinitionName = name,
      typeOpaqueDefinitionParameters = parameters,
      typeOpaqueDefinitionDocComment = Nothing,
      typeOpaqueDefinitionConstructors = map customTypeConstructor constructors
    }

customTypeConstructor :: (Text, [IpeType]) -> CustomTypeConstructor
customTypeConstructor (name, args) =
  CustomTypeConstructor
    { customTypeConstructorName = name,
      customTypeConstructorDocComment = Nothing,
      customTypeConstructorArgs = args
    }
