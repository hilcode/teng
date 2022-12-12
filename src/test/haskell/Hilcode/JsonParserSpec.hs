module Hilcode.JsonParserSpec (spec) where

import Hedgehog (Gen, Property, PropertyT)
import Hilcode.JsonValue (JsonValue (..))
import Hilcode.Key (Key (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Map.Strict
import qualified Data.Vector
import qualified HaskellWorks.Hspec.Hedgehog
import qualified Hedgehog
import qualified Hilcode.JsonParser
import qualified Hilcode.JsonValue.Internal
import qualified Hilcode.Parser

spec :: Spec
spec = describe "JsonParser" $ do
    it "null" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "null" `shouldBe` Right JsonNull
    it "true" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "true" `shouldBe` Right (JsonBoolean True)
    it "false" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "false" `shouldBe` Right (JsonBoolean False)
    it "\"\"" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "\"\"" `shouldBe` Right (JsonString "")
    it "\"\\t\\b\\r\\f\\n\"" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "\"\\t\\b\\r\\f\\n\"" `shouldBe` Right (JsonString "\t\b\r\f\n")
    it "0" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "0" `shouldBe` Right (JsonInteger 0)
    it "-0" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "-0" `shouldBe` Right (JsonInteger 0)
    it "+0" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "+0" `shouldBe` Right (JsonInteger 0)
    it "0.0" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "0.0" `shouldBe` Right (JsonDouble 0)
    it "-0.0" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "-0.0" `shouldBe` Right (JsonDouble 0)
    it "+0.0" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "+0.0" `shouldBe` Right (JsonDouble 0)
    it "0.0e5" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "0.0e5" `shouldBe` Right (JsonDouble 0)
    it "-0.0e-1" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "-0.0e-1" `shouldBe` Right (JsonDouble 0)
    it "+0.0E2" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "+0.0E2" `shouldBe` Right (JsonDouble 0)
    it "01000" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "01000" `shouldBe` Right (JsonInteger 1000)
    it "-01001" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "-01001" `shouldBe` Right (JsonInteger $ negate 1001)
    it "+023456789" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "+023456789" `shouldBe` Right (JsonInteger 23456789)
    it "0.01" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "0.01" `shouldBe` Right (JsonDouble 0.01)
    it "-0.02345" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "-0.02345" `shouldBe` Right (JsonDouble $ negate 0.02345)
    it "+0.05555" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "+0.05555" `shouldBe` Right (JsonDouble 0.05555)
    it "0.0123456e5" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "0.0123456e5" `shouldBe` Right (JsonDouble 0.0123456e5)
    it "-0.06666e-1" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "-0.06666e-1" `shouldBe` Right (JsonDouble $ negate 0.06666e-1)
    it "+0.06E+2" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "+0.06E+2" `shouldBe` Right (JsonDouble 0.06e2)
    it "[]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[]" `shouldBe` Right (JsonArray Data.Vector.empty)
    it "[null]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[null]" `shouldBe` Right (JsonArray $ Data.Vector.singleton JsonNull)
    it "[true]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[true]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonBoolean True)
    it "[false]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[false]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonBoolean False)
    it "[0]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[0]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonInteger 0)
    it "[0.0]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[0.0]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonDouble 0.0)
    it "[\"\"]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[\"\"]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonString "")
    it "[\"abc\"]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[\"abc\"]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonString "abc")
    it "[{\" \":null}]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[{\" \":null}]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonObject $ Data.Map.Strict.fromList [(Key " ", JsonNull)])
    it "[{\"key\":null}]" $
        Hilcode.Parser.run Hilcode.JsonParser.parseValue "[{\"key\":null}]" `shouldBe` Right (JsonArray $ Data.Vector.singleton $ JsonObject $ Data.Map.Strict.fromList [(Key "key", JsonNull)])
    it "Parse JSON string" $
        HaskellWorks.Hspec.Hedgehog.require $ propertyJsonWithoutWhitespace Hilcode.JsonValue.Internal.generateJsonString
    it "Parse JSON number" $
        HaskellWorks.Hspec.Hedgehog.require $ propertyJsonWithoutWhitespace Hilcode.JsonValue.Internal.generateJsonNumber
    it "Parse JSON array" $
        HaskellWorks.Hspec.Hedgehog.require $ propertyJsonWithoutWhitespace Hilcode.JsonValue.Internal.generateJsonArray
    it "Parse JSON object" $
        HaskellWorks.Hspec.Hedgehog.require $ propertyJsonWithoutWhitespace Hilcode.JsonValue.Internal.generateJsonObject
    it "Parse JSON without whitespace" $
        HaskellWorks.Hspec.Hedgehog.require $ propertyJsonWithoutWhitespace Hilcode.JsonValue.Internal.generateJsonValue
    it "Parse JSON" $
        HaskellWorks.Hspec.Hedgehog.require $ propertyJson Hilcode.JsonValue.Internal.generateJsonValue

propertyJsonWithoutWhitespace :: Gen JsonValue -> Property
propertyJsonWithoutWhitespace generator = Hedgehog.property $ do
    jsonValue <- Hedgehog.forAll generator
    checkJsonResult jsonValue (show jsonValue)

propertyJson :: Gen JsonValue -> Property
propertyJson generator = Hedgehog.property $ do
    (jsonValue, json) <- Hedgehog.forAll $ addWhitespace generator
    checkJsonResult jsonValue json
  where
    addWhitespace :: Gen JsonValue -> Gen (JsonValue, String)
    addWhitespace generator = do
        jsonValue <- generator
        json <- Hilcode.JsonValue.Internal.toJsonWithRandomWhitespace jsonValue
        pure (jsonValue, json)

checkJsonResult :: JsonValue -> String -> PropertyT IO ()
checkJsonResult jsonValue json =
    case Hilcode.Parser.run Hilcode.JsonParser.parseValue json of
        Left _ ->
            Hedgehog.assert False
        Right jsonValue' ->
            Hedgehog.assert $ jsonValue == jsonValue'
