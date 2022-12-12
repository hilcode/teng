import Test.Hspec

import qualified Hilcode.DataType.InternalSpec
import qualified Hilcode.FieldName.InternalSpec
import qualified Hilcode.JsonParserSpec
import qualified Hilcode.MiscSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "DataType"    Hilcode.DataType.InternalSpec.spec
    describe "FieldName"   Hilcode.FieldName.InternalSpec.spec
    describe "JsonParser"  Hilcode.JsonParserSpec.spec
    describe "Misc"        Hilcode.MiscSpec.spec
