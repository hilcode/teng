module Hilcode.JsonValue.Internal where

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Hedgehog (Gen)
import Hedgehog.Range (Range)
import Hilcode.Key (Key (..))

import qualified Data.Char
import qualified Data.List
import qualified Data.Map.Strict
import qualified Data.Vector
import qualified Hedgehog.Gen
import qualified Hedgehog.Range

data JsonValue
    = JsonNull
    | JsonBoolean Bool
    | JsonString String
    | JsonInteger Integer
    | JsonDouble Double
    | JsonArray (Vector JsonValue)
    | JsonObject (Map Key JsonValue)
    deriving (Eq)

instance Show JsonValue where
    show JsonNull =
        "null"
    show (JsonBoolean True) =
        "true"
    show (JsonBoolean False) =
        "false"
    show (JsonString text) =
        '"' : text ++ ['"']
    show (JsonInteger number) =
        show number
    show (JsonDouble number) =
        show number
    show (JsonArray array) =
        '[' : Data.List.intercalate "," (Data.Vector.toList $ show <$> array) ++ [']']
    show (JsonObject fields) =
        '{' : Data.List.intercalate "," (fieldToString <$> Data.Map.Strict.toList fields) ++ ['}']
      where
        fieldToString :: (Key, JsonValue) -> String
        fieldToString (key, jsonValue) =
            show key ++ ":" ++ show jsonValue

data JsonToken
    = JsonTokenWhitespace
    | JsonTokenNull
    | JsonTokenBoolean Bool
    | JsonTokenString String
    | JsonTokenInteger Integer
    | JsonTokenDouble Double
    | JsonTokenLeftBrace
    | JsonTokenKey Key
    | JsonTokenColon
    | JsonTokenComma
    | JsonTokenRightBrace
    | JsonTokenLeftBracket
    | JsonTokenRightBracket

toTokens :: JsonValue -> [JsonToken]
toTokens jsonValue = reverse (JsonTokenWhitespace : go [] jsonValue)
  where
    go :: [JsonToken] -> JsonValue -> [JsonToken]
    go tokens JsonNull =
        JsonTokenWhitespace : JsonTokenNull : tokens
    go tokens (JsonBoolean boolean) =
        JsonTokenWhitespace : JsonTokenBoolean boolean : tokens
    go tokens (JsonString text) =
        JsonTokenWhitespace : JsonTokenString text : tokens
    go tokens (JsonInteger integer) =
        JsonTokenWhitespace : JsonTokenInteger integer : tokens
    go tokens (JsonDouble double) =
        JsonTokenWhitespace : JsonTokenDouble double : tokens
    go tokens (JsonArray array) =
        undefined
      where
        _x1 = JsonTokenWhitespace : JsonTokenLeftBracket : tokens
        tokens' :: [JsonToken] -> [JsonValue] -> [JsonToken]

    -- go (JsonArray jsonValues) tokens
    --     = tokens <> [JsonTokenWhitespace, JsonTokenLeftBracket] <> tokens' (Data.Vector.toList jsonValues) <> [JsonTokenWhitespace, JsonTokenRightBracket]
    --   where
    --     tokens' :: [JsonValue] -> [JsonToken]
    --     tokens' [] = []
    --     tokens' [element] = go element
    --     tokens' (element:elements) = go element <> [JsonTokenComma] <> tokens' elements
    -- go (JsonObject record) tokens
    --     = tokens <> [JsonTokenWhitespace, JsonTokenLeftBrace] <> tokens' (Data.Map.Strict.toList record) <> [JsonTokenWhitespace, JsonTokenRightBrace]
    --   where
    --     tokens' :: [(Key, JsonValue)] -> [JsonToken]
    --     tokens' [] = []
    --     tokens' [field] = keyValueToTokens field
    --     tokens' (field:fields) = keyValueToTokens field <> [JsonTokenWhitespace, JsonTokenComma] <> tokens' fields

    --     keyValueToTokens :: (Key, JsonValue) -> [JsonToken]
    --     keyValueToTokens (key, jsonValue) =
    --         [JsonTokenWhitespace, JsonTokenKey key, JsonTokenWhitespace, JsonTokenColon, JsonTokenWhitespace] <> go jsonValue
    go _ _ = undefined

toJsonWithRandomWhitespace :: JsonValue -> Gen String
toJsonWithRandomWhitespace jsonValue = concat <$> sequence (token2String <$> toTokens jsonValue)
  where
    token2String :: JsonToken -> Gen String
    token2String JsonTokenWhitespace =
        generateWhitespace
    token2String JsonTokenNull =
        pure "null"
    token2String (JsonTokenBoolean True) =
        pure "true"
    token2String (JsonTokenBoolean False) =
        pure "false"
    token2String (JsonTokenString text) =
        pure ("\"" <> text <> "\"")
    token2String (JsonTokenInteger integer) =
        pure (show integer)
    token2String (JsonTokenDouble double) =
        pure (show double)
    token2String JsonTokenLeftBrace =
        pure "{"
    token2String (JsonTokenKey (Key key)) =
        pure ("\"" <> key <> "\"")
    token2String JsonTokenColon =
        pure ":"
    token2String JsonTokenComma =
        pure ","
    token2String JsonTokenRightBrace =
        pure "}"
    token2String JsonTokenLeftBracket =
        pure "["
    token2String JsonTokenRightBracket =
        pure "]"

generateWhitespace :: Gen String
generateWhitespace = Hedgehog.Gen.list (Hedgehog.Range.linear 0 4) $ Hedgehog.Gen.choice whitespaceChars
  where
    whitespaceChars :: [Gen Char]
    whitespaceChars = Hedgehog.Gen.constant <$> [' ', '\t', '\n']

generateJsonValue :: Gen JsonValue
generateJsonValue =
    Hedgehog.Gen.recursive
        Hedgehog.Gen.choice
        [ generateJsonNull
        , generateJsonBoolean
        , generateJsonString
        , generateJsonNumber
        ]
        [ generateJsonArray
        , generateJsonObject
        ]

generateJsonNull :: Gen JsonValue
generateJsonNull =
    pure JsonNull

generateJson :: Gen JsonValue -> Gen (JsonValue, String)
generateJson genJsonValue = do
    jsonValue <- genJsonValue
    json <- generateWhitespace <> pure (show jsonValue) <> generateWhitespace
    pure (jsonValue, json)

generateJsonBoolean :: Gen JsonValue
generateJsonBoolean =
    Hedgehog.Gen.choice
        [ pure $ JsonBoolean True
        , pure $ JsonBoolean False
        ]

generateKey :: Gen Key
generateKey = Key <$> generateString

generateJsonString :: Gen JsonValue
generateJsonString = JsonString <$> generateString

generateString :: Gen String
generateString = concat <$> Hedgehog.Gen.list (Hedgehog.Range.linear 0 20) generateChar
  where
    generateUnicode :: Gen String
    generateUnicode = do
        d1 <- Hedgehog.Gen.hexit
        d2 <- Hedgehog.Gen.hexit
        d3 <- Hedgehog.Gen.hexit
        d4 <- Hedgehog.Gen.hexit
        pure ['\\', 'u', d1, d2, d3, d4]

    generateChar :: Gen String
    generateChar =
        Hedgehog.Gen.frequency
            [ (Data.Char.ord '!' - Data.Char.ord ' ' + 1, pure <$> Hedgehog.Gen.enum ' ' '!')
            , (Data.Char.ord '#' - Data.Char.ord '[' + 1, pure <$> Hedgehog.Gen.enum '#' '[')
            , (Data.Char.ord ']' - Data.Char.ord '~' + 1, pure <$> Hedgehog.Gen.enum ']' '~')
            , (1, pure <$> Hedgehog.Gen.enum '\x00007F' '\x10FFFF')
            ,
                ( 5
                , Hedgehog.Gen.choice
                    [ Hedgehog.Gen.constant "\\\""
                    , Hedgehog.Gen.constant "\\\\"
                    , Hedgehog.Gen.constant "\\/"
                    , Hedgehog.Gen.constant "\\b"
                    , Hedgehog.Gen.constant "\\f"
                    , Hedgehog.Gen.constant "\\n"
                    , Hedgehog.Gen.constant "\\r"
                    , Hedgehog.Gen.constant "\\t"
                    , generateUnicode
                    ]
                )
            ]

generateJsonNumber :: Gen JsonValue
generateJsonNumber =
    Hedgehog.Gen.frequency
        [ (24, generateNegative JsonInteger Hedgehog.Gen.integral)
        , (24, generateNegative JsonDouble Hedgehog.Gen.double)
        , (2, Hedgehog.Gen.constant $ JsonInteger 0)
        , (2, Hedgehog.Gen.constant $ JsonDouble 0.0)
        , (24, generatePositive JsonInteger Hedgehog.Gen.integral)
        , (24, generatePositive JsonDouble Hedgehog.Gen.double)
        ]
  where
    range :: Num a => Range a
    range = Hedgehog.Range.constantFrom 1000 1 10000000000

    generateNegative :: Num number => (number -> JsonValue) -> (Range number -> Gen number) -> Gen JsonValue
    generateNegative toJsonValue toGen = toJsonValue . negate <$> toGen range

    generatePositive :: Num number => (number -> JsonValue) -> (Range number -> Gen number) -> Gen JsonValue
    generatePositive toJsonValue toGen = toJsonValue <$> toGen range

generateJsonArray :: Gen JsonValue
generateJsonArray = JsonArray . Data.Vector.fromList <$> Hedgehog.Gen.list (Hedgehog.Range.linear 0 20) generateJsonValue

generateJsonObject :: Gen JsonValue
generateJsonObject = JsonObject . Data.Map.Strict.fromList <$> Hedgehog.Gen.list (Hedgehog.Range.linear 0 20) generateKeyAndValue
  where
    generateKeyAndValue :: Gen (Key, JsonValue)
    generateKeyAndValue = do
        key <- generateKey
        value <- generateJsonValue
        pure (key, value)
