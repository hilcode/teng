module Hilcode.MiscSpec (spec) where

import Test.Hspec

import Hilcode.Misc ((|>), indexed)

spec :: Spec
spec = describe "Misc" $ do
    it "True |> (: []) --> [True]" $
        True |> (: []) `shouldBe` [True]
    it "indexed []" $
        indexed ([] :: [Int]) `shouldBe` []
    it "indexed ['one']" $
        indexed ["one" :: String] `shouldBe` [(0, "one")]
    it "indexed ['one', 'two']" $
        indexed ["one" :: String, "two"] `shouldBe` [(0, "one"), (1, "two")]
