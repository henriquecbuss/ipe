import qualified Ipe.Parser.ImportSpec
import qualified Ipe.Parser.ModuleDefinitionSpec
import qualified Ipe.Parser.TypeDefinitionSpec
import qualified Ipe.ParserSpec
import qualified Test.Hspec

main :: IO ()
main = do
  Test.Hspec.hspec $ do
    Ipe.ParserSpec.spec
    Ipe.Parser.ModuleDefinitionSpec.spec
    Ipe.Parser.ImportSpec.spec
    Ipe.Parser.TypeDefinitionSpec.spec
