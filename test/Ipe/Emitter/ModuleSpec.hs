{-# LANGUAGE OverloadedStrings #-}

module Ipe.Emitter.ModuleSpec (spec) where

import Ipe.Emitter.Module (emit)
import Ipe.Grammar
import Test.Hspec

spec :: Spec
spec = describe "the module emitter" $ do
  it "emits an empty module" $ do
    let module' =
          Module
            { moduleDefinition =
                ModuleDefinition
                  { exportedDefinitions = [],
                    moduleDefinitionName = "Foo",
                    moduleDefinitionPath = [],
                    moduleDocComment = Nothing
                  },
              moduleImports = [],
              typeDefinitions = [],
              topLevelDefinitions = []
            }
    show (emit module')
      `shouldBe` "export default {}\n\
                 \"

  it "emits a module with a single import" $ do
    let module' =
          Module
            { moduleDefinition =
                ModuleDefinition
                  { exportedDefinitions = [],
                    moduleDefinitionName = "Foo",
                    moduleDefinitionPath = [],
                    moduleDocComment = Nothing
                  },
              moduleImports =
                [ ImportExpression
                    { importedModulePath = [],
                      importedModule = "Foo",
                      importedModuleAlias = Nothing
                    }
                ],
              typeDefinitions = [],
              topLevelDefinitions = []
            }
    show (emit module')
      `shouldBe` "import Foo from './Foo'\n\
                 \\n\
                 \export default {}\n\
                 \"

  it "emits a module with a single import with an alias" $ do
    let module' =
          Module
            { moduleDefinition =
                ModuleDefinition
                  { exportedDefinitions = [],
                    moduleDefinitionName = "Foo",
                    moduleDefinitionPath = [],
                    moduleDocComment = Nothing
                  },
              moduleImports =
                [ ImportExpression
                    { importedModulePath = [],
                      importedModule = "Foo",
                      importedModuleAlias = Just ([], "Bar")
                    }
                ],
              typeDefinitions = [],
              topLevelDefinitions = []
            }
    show (emit module')
      `shouldBe` "import Bar from './Foo'\n\
                 \\n\
                 \export default {}\n\
                 \"

  it "emits a module with a single import with a path" $ do
    let module' =
          Module
            { moduleDefinition =
                ModuleDefinition
                  { exportedDefinitions = [],
                    moduleDefinitionName = "Foo",
                    moduleDefinitionPath = [],
                    moduleDocComment = Nothing
                  },
              moduleImports =
                [ ImportExpression
                    { importedModulePath = ["Foo", "Bar"],
                      importedModule = "Baz",
                      importedModuleAlias = Nothing
                    }
                ],
              typeDefinitions = [],
              topLevelDefinitions = []
            }
    show (emit module')
      `shouldBe` "import Foo_Bar_Baz from './Foo/Bar/Baz'\n\
                 \\n\
                 \export default {}\n\
                 \"

  it "emits a module with a single import with a path and an alias" $ do
    let module' =
          Module
            { moduleDefinition =
                ModuleDefinition
                  { exportedDefinitions = [],
                    moduleDefinitionName = "Foo",
                    moduleDefinitionPath = [],
                    moduleDocComment = Nothing
                  },
              moduleImports =
                [ ImportExpression
                    { importedModulePath = ["Foo", "Bar"],
                      importedModule = "Baz",
                      importedModuleAlias = Just ([], "Qux")
                    }
                ],
              typeDefinitions = [],
              topLevelDefinitions = []
            }
    show (emit module')
      `shouldBe` "import Qux from './Foo/Bar/Baz'\n\
                 \\n\
                 \export default {}\n\
                 \"

  it "emit a module with a single top level definition" $
    let module' =
          Module
            { moduleDefinition =
                ModuleDefinition
                  { exportedDefinitions = [],
                    moduleDefinitionName = "Foo",
                    moduleDefinitionPath = [],
                    moduleDocComment = Nothing
                  },
              moduleImports = [],
              typeDefinitions = [],
              topLevelDefinitions =
                [ TopLevelDefinition
                    { topLevelDefinitionName = "foo",
                      topLevelDefinitionDocComment = Nothing,
                      topLevelDefinitionValue = IpeNumber 42,
                      topLevelDefinitionTypeAnnotation = Nothing
                    }
                ]
            }
     in show (emit module')
          `shouldBe` "const foo = 42.0;\n\
                     \\n\
                     \export default {}\n\
                     \"

  it "emit a module with a single top level definition, that uses an imported value and export it" $
    let module' =
          Module
            { moduleDefinition =
                ModuleDefinition
                  { exportedDefinitions = ["foo"],
                    moduleDefinitionName = "Foo",
                    moduleDefinitionPath = ["Foo"],
                    moduleDocComment = Nothing
                  },
              moduleImports =
                [ ImportExpression
                    { importedModulePath = [],
                      importedModule = "Bar",
                      importedModuleAlias = Nothing
                    }
                ],
              typeDefinitions = [],
              topLevelDefinitions =
                [ TopLevelDefinition
                    { topLevelDefinitionName = "foo",
                      topLevelDefinitionDocComment = Nothing,
                      topLevelDefinitionValue =
                        IpeFunctionCallOrValue
                          ( FunctionCallOrValue
                              { functionCallOrValuePath = ["Bar"],
                                functionCallOrValueName = "bar",
                                functionCallOrValueRecordAccessors = [],
                                functionCallOrValueArguments = []
                              }
                          ),
                      topLevelDefinitionTypeAnnotation = Nothing
                    }
                ]
            }
     in show (emit module')
          `shouldBe` "import Bar from '../Bar'\n\
                     \\n\
                     \const foo = Bar.bar;\n\
                     \\n\
                     \export default {foo}\n\
                     \"
