module Hilcode.JsonParser (
    parseValue,
) where

import Prelude hiding (repeat)

import qualified Data.Char
import qualified Data.Map.Strict
import qualified Data.Vector

import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Ratio ((%))

import Control.Applicative ((<|>))
import Hilcode.Parser (Parser, char', keyword, optional, repeat, repeat1, satisfy)
import Hilcode.Key (Key(..))
import Hilcode.JsonValue (JsonValue(..))

parseJsonNull :: Parser error JsonValue
parseJsonNull =
    keyword "null" $> JsonNull

parseJsonBoolean :: Parser error JsonValue
parseJsonBoolean =
    parseTrue <|> parseFalse
  where
    parseTrue :: Parser error JsonValue
    parseTrue =
        keyword "true" $> JsonBoolean True

    parseFalse :: Parser error JsonValue
    parseFalse =
        keyword "false" $> JsonBoolean False

parseJsonArray :: Parser String JsonValue
parseJsonArray = do
    char' '['
    maybeFirstElement <- optional parseValue
    elements <-
        case maybeFirstElement of
            Nothing ->
                pure []
            Just firstElement -> do
                moreElements <- repeat (char' ',' >> parseValue)
                pure $ firstElement : moreElements
    char' ']'
    pure $ JsonArray $ Data.Vector.fromList elements

parseJsonString :: Parser error JsonValue
parseJsonString = JsonString <$> parseString

parseKey :: Parser error Key
parseKey = Key <$> parseString

parseString :: Parser error String
parseString = do
    char' '"'
    text <- repeat parseChar
    char' '"'
    pure text

parseChar :: Parser error Char
parseChar = parseCharSimple <|> parseCharEscape

parseCharSimple :: Parser error Char
parseCharSimple = satisfy (\ch -> ' ' <= ch && ch <= '\x10ffff' && ch /= '"' && ch /= '\\')

parseCharEscape :: Parser error Char
parseCharEscape = do
    char' '\\'
    escapeChar <- satisfy (\ch -> ch `elem` ['\\', '/', 'b', 'f', 'n', 'r', 't', 'u'])
    case escapeChar of
        '\\' ->
            pure '\\'
        '/' ->
            pure '/'
        'b' ->
            pure '\b'
        'f' ->
            pure '\f'
        'n' ->
            pure '\n'
        'r' ->
            pure '\r'
        't' ->
            pure '\t'
        _ ->
            parseCharEscapeUnicode

parseCharEscapeUnicode :: Parser error Char
parseCharEscapeUnicode = do
    hexit1 <- parseHexit
    hexit2 <- parseHexit
    hexit3 <- parseHexit
    hexit4 <- parseHexit
    pure $ Data.Char.chr $ ((hexit1 * 16 + hexit2) * 16 + hexit3) * 16 + hexit4

parseHexit :: Parser error Int
parseHexit = do
    ch <- satisfy (\ch -> ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F'))
    pure $
        if '0' <= ch && ch <= '9'
            then Data.Char.ord ch - Data.Char.ord '0'
            else
                if 'a' <= ch && ch <= 'f'
                    then Data.Char.ord ch - Data.Char.ord 'a' + 10
                    else Data.Char.ord ch - Data.Char.ord 'A' + 10

data Sign
    = NoSign
    | PlusSign
    | MinusSign

parseSign :: Parser error Sign
parseSign = do
    maybeSign <- optional $ satisfy (\ch -> ch == '-' || ch == '+')
    pure $ case maybeSign of
        Just '-' ->
            MinusSign
        Just _ ->
            PlusSign
        Nothing ->
            NoSign

parseJsonNumber :: Parser error JsonValue
parseJsonNumber = do
    (sign, integer) <- parseInteger
    fraction <- parseFraction
    exponent <- parseExponent
    pure $ toJsonValue sign (integer, fraction, exponent)
  where
    parseInteger :: Parser error (Sign, Integer)
    parseInteger = do
        sign <- parseSign
        (firstDigit, moreDigits) <- repeat1 $ satisfy (\ch -> '0' <= ch && ch <= '9')
        pure (sign, makeInteger (fromIntegral $ Data.Char.digitToInt firstDigit, moreDigits))

    parseFraction :: Parser error (Maybe (Integer, Integer))
    parseFraction = optional $ do
        char' '.'
        (firstDigit, moreDigits) <- repeat1 $ satisfy (\ch -> '0' <= ch && ch <= '9')
        pure (toSignedInteger NoSign (makeInteger (fromIntegral $ Data.Char.digitToInt firstDigit, moreDigits)), foldl' (\i _ -> 10 * i) 10 moreDigits)

    parseExponent :: Parser error (Maybe Integer)
    parseExponent = optional $ do
        char' 'e' <|> char' 'E'
        sign <- parseSign
        (firstDigit, moreDigits) <- repeat1 $ satisfy (\ch -> '0' <= ch && ch <= '9')
        pure $ toSignedInteger sign (makeInteger (fromIntegral $ Data.Char.digitToInt firstDigit, moreDigits))

    toJsonValue :: Sign -> (Integer, Maybe (Integer, Integer), Maybe Integer) -> JsonValue
    toJsonValue sign numberParts =
        case sign of
            NoSign ->
                either JsonInteger JsonDouble $ toJsonValue' numberParts
            PlusSign ->
                either JsonInteger JsonDouble $ toJsonValue' numberParts
            MinusSign ->
                either (JsonInteger . negate) (JsonDouble . negate) $ toJsonValue' numberParts
      where
        toJsonValue' :: (Integer, Maybe (Integer, Integer), Maybe Integer) -> Either Integer Double
        toJsonValue' (integer, Nothing, Nothing) =
            Left integer
        toJsonValue' (integer, Just (fraction, divisor), Nothing) =
            Right $ fromRational $ toRational integer + (fraction % divisor)
        toJsonValue' (integer, Nothing, Just 0) =
            Left integer
        toJsonValue' (integer, Nothing, Just exponent)
            | exponent < 0 =
                Right $ fromRational $ toRational integer * (1 % 10) ^ negate exponent
        toJsonValue' (integer, Nothing, Just exponent) =
            Left $ integer * 10 ^ exponent
        toJsonValue' (integer, Just (fraction, divisor), Just 0) =
            Right $ fromRational $ toRational integer + (fraction % divisor)
        toJsonValue' (integer, Just (fraction, divisor), Just exponent)
            | exponent < 0 =
                Right $ fromRational $ (toRational integer + (fraction % divisor)) * (1 % 10) ^ negate exponent
        toJsonValue' (integer, Just (fraction, divisor), Just exponent) =
            Right $ fromRational $ (toRational integer + (fraction % divisor)) * 10 ^ exponent

toSignedInteger :: Sign -> Integer -> Integer
toSignedInteger sign integer =
    case sign of
        NoSign ->
            integer
        PlusSign ->
            integer
        MinusSign ->
            negate integer

makeInteger :: (Integer, [Char]) -> Integer
makeInteger (accumulator, []) =
    accumulator
makeInteger (accumulator, digit : digits) =
    makeInteger (10 * accumulator + fromIntegral (Data.Char.digitToInt digit), digits)

parseJsonObject :: Parser String JsonValue
parseJsonObject = do
    char' '{'
    maybeFirstField <- optional parseField
    fields <-
        case maybeFirstField of
            Nothing ->
                pure []
            Just firstField -> do
                moreFields <- repeat (char' ',' >> parseField)
                pure $ firstField : moreFields
    char' '}'
    pure $ JsonObject $ Data.Map.Strict.fromList fields
  where
    parseField :: Parser String (Key, JsonValue)
    parseField = do
        key <- parseKey
        char' ':'
        value <- parseValue
        pure (key, value)

parseValue :: Parser String JsonValue
parseValue =
    parseJsonNull <|> parseJsonBoolean <|> parseJsonArray <|> parseJsonString <|> parseJsonNumber <|> parseJsonObject
