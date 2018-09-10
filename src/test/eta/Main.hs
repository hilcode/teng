import           Template
    (Template(..), parse)
import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "teng" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do

    it "a plain divider without EOL should work" $ do
        parse "-----" `shouldBe` Template ""

    it "a plain divider with a UNIX EOL should work" $ do
        parse "-----\n" `shouldBe` Template ""

    it "a plain divider with a Windows EOL should work" $ do
        parse "-----\n\f" `shouldBe` Template ""
