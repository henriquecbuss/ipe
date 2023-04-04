import qualified Ipe.Emitter.ExpressionSpec
import Ipe.Emitter.ModuleSpec
import qualified Ipe.Parser.ExpressionSpec
import qualified Ipe.Parser.ImportSpec
import qualified Ipe.Parser.ModuleDefinitionSpec
import qualified Ipe.Parser.ModuleSpec
import qualified Ipe.Parser.TopLevelDefinitionSpec
import qualified Ipe.Parser.TypeDefinitionSpec
import qualified Ipe.ParserSpec
import qualified Ipe.Transformer.ModuleSpec
import qualified Ipe.TypeChecker.ExpressionSpec
import qualified Ipe.TypeChecker.ModuleSpec
import qualified Test.Hspec

main :: IO ()
main =
  Test.Hspec.hspec $ do
    Ipe.ParserSpec.spec
    Ipe.Parser.ModuleDefinitionSpec.spec
    Ipe.Parser.ImportSpec.spec
    Ipe.Parser.TypeDefinitionSpec.spec
    Ipe.Parser.ExpressionSpec.spec
    Ipe.Parser.TopLevelDefinitionSpec.spec
    Ipe.Parser.ModuleSpec.spec

    Ipe.Transformer.ModuleSpec.spec

    Ipe.TypeChecker.ExpressionSpec.spec
    Ipe.TypeChecker.ModuleSpec.spec

    Ipe.Emitter.ExpressionSpec.spec
    Ipe.Emitter.ModuleSpec.spec
