import qualified Ipe.Parser.ModuleDefinitionSpec
import qualified Ipe.ParserSpec
import qualified Test.Hspec

main :: IO ()
main = do
  Test.Hspec.hspec $ do
    Ipe.ParserSpec.spec
    Ipe.Parser.ModuleDefinitionSpec.spec
