{-# LANGUAGE OverloadedStrings #-}

module Ipe.Transformer.ModuleSpec (spec) where

import Data.Text (Text)
import Ipe.Grammar (CustomTypeConstructor (..), IpeType (..), Module (..), ModuleDefinition (..), TypeDefinition (..), TypeOpaque (..), TypeUnion (..))
import qualified Ipe.Transformer.Module
import Test.Hspec

spec :: Spec
spec = describe "the module transformer" $ do
  exportTypeUnionConstructors

exportTypeUnionConstructors :: Spec
exportTypeUnionConstructors =
  describe "when exporting type union constructors" $ do
    it "should not add anything if no type is exported" $
      exportedDefinitions
        ( moduleDefinition
            (Ipe.Transformer.Module.apply sampleModule)
        )
        `shouldBe` []

    it "should correctly add the constructors to the export list" $
      exportedDefinitions
        ( moduleDefinition
            ( Ipe.Transformer.Module.apply
                ( sampleModule
                    { moduleDefinition =
                        sampleModuleDefinition
                          { exportedDefinitions = ["UnionTest"]
                          }
                    }
                )
            )
        )
        `shouldBe` ["UnionTest", "A", "B", "C"]

    it "should not add the opaque type constructors" $
      exportedDefinitions
        ( moduleDefinition
            ( Ipe.Transformer.Module.apply
                ( sampleModule
                    { moduleDefinition =
                        sampleModuleDefinition
                          { exportedDefinitions = ["UnionTest", "OpaqueTest"]
                          }
                    }
                )
            )
        )
        `shouldBe` ["UnionTest", "OpaqueTest", "A", "B", "C"]

sampleModule :: Module
sampleModule =
  Module
    { moduleDefinition = sampleModuleDefinition,
      moduleImports = [],
      typeDefinitions =
        [ TypeUnionDefinition typeUnion0,
          TypeOpaqueDefinition typeOpaque0
        ],
      topLevelDefinitions = []
    }

typeUnion0 :: TypeUnion
typeUnion0 =
  TypeUnion
    { typeUnionDefinitionName = "UnionTest",
      typeUnionDefinitionParameters = ["a", "b"],
      typeUnionDefinitionDocComment = Nothing,
      typeUnionDefinitionConstructors =
        map
          customTypeConstructor
          [ ("A", [ConcreteType [] "Number" []]),
            ("B", [ConcreteType [] "String" [], ConcreteType [] "Number" [], ParameterType "b"]),
            ("C", [ConcreteType [] "OpaqueTest" [ParameterType "a", ParameterType "b"]])
          ]
    }

typeOpaque0 :: TypeOpaque
typeOpaque0 =
  TypeOpaque
    { typeOpaqueDefinitionName = "OpaqueTest",
      typeOpaqueDefinitionParameters = ["a", "b"],
      typeOpaqueDefinitionDocComment = Nothing,
      typeOpaqueDefinitionConstructors =
        map
          customTypeConstructor
          [ ("OA", [ParameterType "a"]),
            ("OB", [ParameterType "b"])
          ]
    }

customTypeConstructor :: (Text, [IpeType]) -> CustomTypeConstructor
customTypeConstructor (name, args) =
  CustomTypeConstructor
    { customTypeConstructorName = name,
      customTypeConstructorDocComment = Nothing,
      customTypeConstructorArgs = args
    }

sampleModuleDefinition :: ModuleDefinition
sampleModuleDefinition =
  ModuleDefinition
    { moduleDefinitionPath = [],
      moduleDefinitionName = "Test",
      exportedDefinitions = [],
      moduleDocComment = Nothing
    }
