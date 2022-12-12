module Main where

import Prelude hiding (repeat)

import qualified Data.Map.Strict

import Hilcode.DataType (DataType(..))
import Hilcode.FieldName (FieldName(..))
import Hilcode.Parser (Failure(..), run)
import Hilcode.JsonParser (parseValue)

main :: IO ()
main =
    case jsonValue of
        Left NoMatch ->
            putStrLn "No match"

        Left _ ->
            putStrLn "ERROR"

        Right jsonValue ->
            print jsonValue

  where
    json = "\" \""
    jsonValue = run parseValue json

main2 :: IO ()
main2 = do
    print TypeString
    print TypeInt
    print $
        TypeRecord (Data.Map.Strict.fromList
            [ (FieldName "field-1", TypeString)
            ])
    print $
        TypeRecord (Data.Map.Strict.fromList
            [ (FieldName "field-1", TypeString)
            , (FieldName "field-2", TypeInt)
            ])
    print $
        TypeRecord (Data.Map.Strict.fromList
            [ (FieldName "field-1", TypeString)
            , (FieldName "field-2", TypeInt)
            , (FieldName "field-3", TypeRecord (Data.Map.Strict.fromList
                [ (FieldName "field-1", TypeString)
                , (FieldName "field-2", TypeInt)
                , (FieldName "field-3", TypeInt)
                ]))
            ])
    print $
        TypeRecord (Data.Map.Strict.fromList
            [ (FieldName "field-1", TypeString)
            , (FieldName "field-2", TypeInt)
            , (FieldName "field-3", TypeRecord (Data.Map.Strict.fromList
                [ (FieldName "field-1", TypeString)
                , (FieldName "field-2", TypeInt)
                , (FieldName "field-3", TypeInt)
                ]))
            , (FieldName "field-4", TypeRecord (Data.Map.Strict.fromList
                [ (FieldName "field-1", TypeString)
                , (FieldName "field-2", TypeInt)
                , (FieldName "field-3", TypeRecord (Data.Map.Strict.fromList
                    [ (FieldName "field-1", TypeString)
                    , (FieldName "field-2", TypeInt)
                    , (FieldName "field-3", TypeInt)
                    ]))
                ]))
            ])
