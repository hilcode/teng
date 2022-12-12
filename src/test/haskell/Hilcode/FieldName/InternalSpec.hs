module Hilcode.FieldName.InternalSpec (spec) where

import Hedgehog.Classes ( lawsCheck, eqLaws, ordLaws )
import Test.Hspec ( describe, it, shouldBe, shouldReturn, Spec )

import Hilcode.FieldName.Internal (FieldName(..))

import qualified Hilcode.FieldName.Internal

spec :: Spec
spec = describe "FieldName" $ do
    it "Eq" $
        lawsCheck (eqLaws Hilcode.FieldName.Internal.generate) `shouldReturn` True
    it "Ord" $
        lawsCheck (ordLaws Hilcode.FieldName.Internal.generate) `shouldReturn` True
    it "show $ FieldName 'field-name'" $
        show (FieldName "field-name") `shouldBe` "FieldName \"field-name\""
