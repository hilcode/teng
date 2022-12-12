module Hilcode.DataType.InternalSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict

import Hedgehog (forAll, property, Property, assert)
import Hilcode.DataType.Internal
    ( DataType(..),
      satisfies,
      toValue,
      toReference,
      generateDataTypeAndReferenceDataType,
      Reference )
import Hilcode.FieldName.Internal (FieldName(..))
import qualified HaskellWorks.Hspec.Hedgehog

x :: FieldName
x = FieldName "x"

y :: FieldName
y = FieldName "y"

z :: FieldName
z = FieldName "z"

one :: FieldName
one = FieldName "one"

two :: FieldName
two = FieldName "two"

three :: FieldName
three = FieldName "three"

pointX :: DataType Reference
pointX = TypeRecord (Data.Map.Strict.singleton x TypeInt)

pointXY :: DataType Reference
pointXY = TypeRecord (Data.Map.Strict.fromList [(x, TypeInt), (y, TypeInt)])

pointXYZ :: DataType Reference
pointXYZ = TypeRecord (Data.Map.Strict.fromList [(x, TypeInt), (y, TypeInt), (z, TypeInt)])

record1 :: DataType Reference
record1 = TypeRecord (Data.Map.Strict.fromList [(one, pointX)])

record12 :: DataType Reference
record12 = TypeRecord (Data.Map.Strict.fromList [(one, pointX), (two, pointXY)])

record123 :: DataType Reference
record123 = TypeRecord (Data.Map.Strict.fromList [(one, pointX), (two, pointXY), (three, pointXYZ)])

record :: DataType Reference
record = TypeRecord (Data.Map.Strict.fromList [(one, pointX), (two, pointX), (three, pointX)])

propertyDataType :: Property
propertyDataType = property $ do
    (dataType, referenceDataType) <- forAll generateDataTypeAndReferenceDataType
    assert $ (dataType `satisfies` referenceDataType) && not (toValue referenceDataType `satisfies` toReference dataType)

spec :: Spec
spec = do
    describe "satisfies :: TypeString" $ do
        it "TypeString `satisfies` TypeString" $
            TypeString `satisfies` TypeString `shouldBe` True
        it "TypeString `satisfies` TypeInt" $
            TypeString `satisfies` TypeInt `shouldBe` False
    describe "satisfies :: TypeInt" $ do
        it "TypeInt `satisfies` TypeInt" $
            satisfies TypeInt TypeInt `shouldBe` True
        it "TypeInt `satisfies` TypeString" $
            TypeInt `satisfies` TypeString `shouldBe` False
    describe "satisfies :: TypeRecord" $ do
        it "pointX `satisfies` pointX" $
            toValue pointX `satisfies` pointX `shouldBe` True
        -- it "pointX `satisfies` pointXY" $
        --     pointX `satisfies` pointXY `shouldBe` False
        -- it "pointXY `satisfies` pointX" $
        --     pointXY `satisfies` pointX `shouldBe` True
        -- it "pointXY `satisfies` pointXY" $
        --     pointXY `satisfies` pointXY `shouldBe` True
        -- it "pointX `satisfies` pointXYZ" $
        --     pointX `satisfies` pointXYZ `shouldBe` False
        -- it "pointXYZ `satisfies` pointX" $
        --     pointXYZ `satisfies` pointX `shouldBe` True
        -- it "pointXY `satisfies` pointXYZ" $
        --     pointXY `satisfies` pointXYZ `shouldBe` False
        -- it "pointXYZ `satisfies` pointXY" $
        --     pointXYZ `satisfies` pointXY `shouldBe` True
        -- it "pointXYZ `satisfies` pointXYZ" $
        --     pointXYZ `satisfies` pointXYZ `shouldBe` True
        -- it "record123 `satisfies` record1" $
        --     record123 `satisfies` record1 `shouldBe` True
        -- it "record1 `satisfies` record123" $
        --     record1 `satisfies` record123 `shouldBe` False
        -- it "record123 `satisfies` record12" $
        --     record123 `satisfies` record12 `shouldBe` True
        -- it "record12 `satisfies` record123" $
        --     record12 `satisfies` record123 `shouldBe` False
        -- it "record123 `satisfies` record123" $
        --     record123 `satisfies` record123 `shouldBe` True
        -- it "record1 `satisfies` record" $
        --     record1 `satisfies` record `shouldBe` False
        -- it "record `satisfies` record1" $
        --     record `satisfies` record1 `shouldBe` True
        -- it "record `satisfies` record12" $
        --     record `satisfies` record12 `shouldBe` True
        -- it "record12 `satisfies` record" $
        --     record12 `satisfies` record `shouldBe` False
        -- it "record `satisfies` record123" $
        --     record `satisfies` record123 `shouldBe` True
        -- it "record123 `satisfies` record" $
        --     record123 `satisfies` record `shouldBe` False
    describe "satisfies :: TypeRecord 2" $ do
        it "dataType `satisfies` referenceDataType" $
            HaskellWorks.Hspec.Hedgehog.require propertyDataType
