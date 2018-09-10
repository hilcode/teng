import qualified Data.Sequence    as Sequence
import           Template
    (Eol(..), Line(..), Template(..), parse)
import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "teng" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do

    it "a plain divider without EOL should work" $ do
        parse "-----" `shouldBe` Template Sequence.empty

    it "a plain divider with a UNIX EOL should work" $ do
        parse "-----\n" `shouldBe` Template Sequence.empty

    it "a plain divider with a Windows EOL should work" $ do
        parse "-----\r\n" `shouldBe` Template Sequence.empty

    it "a single line without EOL should work" $ do
        parse "-----\nHello World!" `shouldBe` Template (Sequence.singleton (Line "Hello World!" EOF))

    it "a single line with UNIX EOL should work" $ do
        parse "-----\nHello World!\n" `shouldBe` Template (Sequence.singleton (Line "Hello World!" LF))

    it "a single line with Windows EOL should work" $ do
        parse "-----\nHello World!\r\n" `shouldBe` Template (Sequence.singleton (Line "Hello World!" CR_LF))

    it "two lines without EOL should work" $ do
        parse "-----\nHello\nWorld!" `shouldBe` Template (Sequence.fromList [Line "Hello" LF, Line "World!" EOF])

    it "two lines with UNIX EOL should work" $ do
        parse "-----\nHello\nWorld!\n" `shouldBe` Template (Sequence.fromList [Line "Hello" LF, Line "World!" LF])

    it "two lines with Windows EOL should work" $ do
        parse "-----\nHello\r\nWorld!\r\n" `shouldBe` Template (Sequence.fromList [Line "Hello" CR_LF, Line "World!" CR_LF])
