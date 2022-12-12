module Hilcode.DataType.Internal where

import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Data.Text

import Data.Map.Strict (Map)
import Data.Monoid (All(..))
import Data.Text (Text)
import Hedgehog (Gen)
import Hedgehog.Range (Range)
import Hedgehog.Gen
import qualified Hedgehog.Range

import Hilcode.FieldName (FieldName(..))
import Hilcode.Misc ((|>), indexed)

data Reference = Reference
data Value = Value

data DataType a
    = TypeString
    | TypeInt
    | TypeRecord (Map FieldName (DataType a))
    deriving (Eq)

toReference :: DataType Value -> DataType Reference
toReference dataType
    = case dataType of
        TypeString ->
            TypeString
        TypeInt ->
            TypeInt
        TypeRecord record ->
            TypeRecord $ toReference <$> record

toValue :: DataType Reference -> DataType Value
toValue dataType
    = case dataType of
        TypeString ->
            TypeString
        TypeInt ->
            TypeInt
        TypeRecord record ->
            TypeRecord $ toValue <$> record

instance Show (DataType any) where
    show = showDataType ""
{-
{ field-1: String
, field-2: Int
, field-3:
    { field-1:
        { field-1: Int
        , field-2: String
        }
    , field-2:
        { field-1: String
        }
    , field-3: Int
    , field-4:
        { field-1: Int
        , field-2: Int
        , field-3: String
        }
    }
}
-}

showField :: String -> (Bool, FieldName, DataType any) -> String
showField currentIndentation (True, FieldName fieldName, TypeString)
    = currentIndentation <> "{ " <> Data.Text.unpack fieldName <> ": String"
showField currentIndentation (False, FieldName fieldName, TypeString)
    = currentIndentation <> ", " <> Data.Text.unpack fieldName <> ": String"
showField currentIndentation (True, FieldName fieldName, TypeInt)
    = currentIndentation <> "{ " <> Data.Text.unpack fieldName <> ": Int"
showField currentIndentation (False, FieldName fieldName, TypeInt)
    = currentIndentation <> ", " <> Data.Text.unpack fieldName <> ": Int"
showField currentIndentation (True, FieldName fieldName, dataType)
    = currentIndentation <> "{ " <> Data.Text.unpack fieldName <> ":\n" <> showDataType (currentIndentation ++ "  ") dataType
showField currentIndentation (False, FieldName fieldName, dataType)
    = currentIndentation <> ", " <> Data.Text.unpack fieldName <> ":\n" <> showDataType (currentIndentation ++ "  ") dataType

showDataType :: forall any. String -> DataType any -> String
showDataType _ TypeString
    = "String"
showDataType _ TypeInt
    = "Int"
showDataType currentIndentation (TypeRecord record)
    = ("\n" `Data.List.intercalate` _x4) <> "\n" <> currentIndentation <> "}"
  where
    _x1 :: [(FieldName, DataType any)]
    _x1 = Data.Map.Strict.toList record
    _x2 :: [(Bool, FieldName, DataType any)]
    _x2
        = case _x1 of
            [] ->
                []
            (fieldName, dataType) : rest ->
                (True, fieldName, dataType) : (_x3 <$> rest)
    _x3 :: (FieldName, DataType any) -> (Bool, FieldName, DataType any)
    _x3 (fieldName, dataType) = (False, fieldName, dataType)
    _x4 :: [String]
    _x4 = showField currentIndentation <$> _x2

satisfies :: DataType Value -> DataType Reference -> Bool
satisfies dataType referenceDataType
    = case dataType of
        TypeString ->
            referenceDataType == TypeString

        TypeInt ->
            referenceDataType == TypeInt

        TypeRecord record ->
            case referenceDataType of
                TypeRecord referenceRecord ->
                    foldMap (All . checkReferenceField) (Data.Map.Strict.toList referenceRecord)
                        |> getAll
                  where
                    checkReferenceField :: (FieldName, DataType Reference) -> Bool
                    checkReferenceField (referenceFieldName, referenceDataType)
                        = Data.Maybe.maybe False (`satisfies` referenceDataType) (Data.Map.Strict.lookup referenceFieldName record)

                _ ->
                    False

generate :: Gen (DataType any)
generate
    = recursive choice [ constant TypeString, constant TypeInt ] [ generateRecord ]

generateRecord :: forall any. Gen (DataType any)
generateRecord = do
    fields <- fmap addFieldName . indexed <$> list fieldCount generate
    pure $ TypeRecord $ Data.Map.Strict.fromList fields
  where
    fieldCount :: Range Int
    fieldCount = Hedgehog.Range.linear 1 6

    addFieldName :: (Int, DataType any) -> (FieldName, DataType any)
    addFieldName (index, dataType) = (FieldName (("field-" :: Text) <> Data.Text.pack (show index)), dataType)

generateDataTypeAndReferenceDataType :: Gen (DataType Value, DataType Reference)
generateDataTypeAndReferenceDataType = do
    referenceDataType :: DataType Reference <- generateRecord
    dataType <- expand referenceDataType
    pure (toValue dataType, referenceDataType)

expand :: DataType any -> Gen (DataType any)
expand dataType
    = case dataType of
        TypeString ->
            constant dataType

        TypeInt ->
            constant dataType

        TypeRecord record -> do
            extraDataType <- generate
            pure $ TypeRecord (Data.Map.Strict.insert (FieldName "extra-field") extraDataType record)

expandFields :: forall any. DataType any -> Gen (DataType any)
expandFields dataType
    = case dataType of
        TypeString ->
            constant dataType

        TypeInt ->
            constant dataType

        TypeRecord record ->
            let
                _z1 :: [(FieldName, DataType any)]
                _z1 = Data.Map.Strict.toList record

                _z2 :: Gen [(FieldName, DataType any)]
                _z2 = _x2 _z1
            in
                _z2 >>= _x3
  where
    _x1 :: (FieldName, DataType any) -> Gen (FieldName, DataType any)
    _x1 (fieldName, dataType) = do
        expandedDataType <- expand dataType
        pure (fieldName, expandedDataType)

    _x2 :: [(FieldName, DataType any)] -> Gen [(FieldName, DataType any)]
    _x2 as
        = case as of
            [] ->
                pure []
            fieldNameAndDataType : rest ->
                let
                    y1 :: Gen (FieldName, DataType any)
                    y1 = _x1 fieldNameAndDataType
                    y2 :: Gen [(FieldName, DataType any)]
                    y2 = _x2 rest
                    y3 :: Gen [(FieldName, DataType any)]
                    y3 = do
                        _y1 <- y1
                        _y2 <- y2
                        pure $ _y1 : _y2
                in
                    y3

    _x3 :: [(FieldName, DataType any)] -> Gen (DataType any)
    _x3 fields = pure (TypeRecord (Data.Map.Strict.fromList fields))
