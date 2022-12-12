module Hilcode.Parser (
    Parser,
    Failure (..),
    char,
    char',
    commit,
    keyword,
    optional,
    repeat,
    repeat1,
    run,
    satisfy,
    string,
) where

import Prelude hiding (repeat)

import Control.Applicative (Alternative (..), Applicative (..))

data Failure error
    = NoMatch
    | Error error [error]
    deriving (Show, Eq)

newtype Parser error result
    = Parser ([Char] -> Either (Failure error) ([Char], result))

instance Functor (Parser error) where
    fmap ::
        (a -> b) ->
        Parser error a ->
        Parser error b
    function `fmap` (Parser parse) = Parser $ (fmap . fmap . fmap) function parse

instance Applicative (Parser error) where
    pure ::
        a ->
        Parser error a
    pure result =
        Parser (\text -> success (text, result))

    liftA2 ::
        forall a b c.
        (a -> b -> c) ->
        Parser error a ->
        Parser error b ->
        Parser error c
    liftA2 function (Parser parseA) (Parser parseB) =
        Parser parse
      where
        parse ::
            [Char] ->
            Either (Failure error) ([Char], c)
        parse textA =
            case parseA textA of
                Left failure ->
                    Left failure
                Right (textB, a) ->
                    case parseB textB of
                        Left failure ->
                            Left failure
                        Right (textC, b) ->
                            success (textC, function a b)

instance Monad (Parser error) where
    (>>=) ::
        forall a b.
        Parser error a ->
        (a -> Parser error b) ->
        Parser error b
    (Parser parseA) >>= function =
        Parser parse
      where
        parse ::
            [Char] ->
            Either (Failure error) ([Char], b)
        parse textA =
            case parseA textA of
                Left failure ->
                    Left failure
                Right (textB, a) ->
                    case function a of
                        Parser parseB ->
                            parseB textB

instance forall result error. Semigroup result => Semigroup (Parser error result) where
    (<>) ::
        Parser error result ->
        Parser error result ->
        Parser error result
    lhs <> rhs = do
        lhr <- lhs
        rhr <- rhs
        pure (lhr <> rhr)

instance forall result error. Monoid result => Monoid (Parser error result) where
    mempty :: Parser error result
    mempty = Parser $ const $ Right mempty

instance Alternative (Parser error) where
    empty :: Parser error result
    empty = Parser $ const $ Left NoMatch

    (<|>) ::
        Parser error result ->
        Parser error result ->
        Parser error result
    Parser lhs <|> Parser rhs = Parser parse
      where
        parse text =
            case lhs text of
                failure@(Left (Error _ _)) ->
                    failure
                Left NoMatch ->
                    rhs text
                success@(Right _) ->
                    success

run ::
    Parser error result ->
    String ->
    Either (Failure error) result
run (Parser parse) text =
    case parse text of
        Left failure ->
            Left failure
        Right (text', result) ->
            if text' == ""
                then Right result
                else Left NoMatch

success ::
    result ->
    Either error result
success = Right

failure ::
    error ->
    Either error result
failure = Left

char ::
    Char ->
    Parser error Char
char expectedChar =
    Parser parse
  where
    parse ::
        [Char] ->
        Either (Failure error) ([Char], Char)
    parse text =
        case text of
            [] ->
                failure NoMatch
            actualChar : rest ->
                if actualChar == expectedChar
                    then success (rest, actualChar)
                    else failure NoMatch

char' ::
    Char ->
    Parser error ()
char' expectedChar =
    Parser parse
  where
    parse ::
        [Char] ->
        Either (Failure error) ([Char], ())
    parse text =
        case text of
            [] ->
                failure NoMatch
            actualChar : rest ->
                if actualChar == expectedChar
                    then success (rest, ())
                    else failure NoMatch

satisfy ::
    (Char -> Bool) ->
    Parser error Char
satisfy predicate =
    Parser parse
  where
    parse ::
        [Char] ->
        Either (Failure error) ([Char], Char)
    parse text =
        case text of
            [] ->
                failure NoMatch
            actualChar : rest ->
                if predicate actualChar
                    then success (rest, actualChar)
                    else failure NoMatch

string ::
    String ->
    Parser error String
string text =
    sequence $ char <$> text

keyword ::
    String ->
    Parser error ()
keyword text =
    sequence_ $ char' <$> text

repeat ::
    forall error result.
    Parser error result ->
    Parser error [result]
repeat (Parser parseValue) =
    Parser $ parse []
  where
    parse ::
        [result] ->
        [Char] ->
        Either (Failure error) ([Char], [result])
    parse results text =
        case parseValue text of
            Left failure_ ->
                case failure_ of
                    NoMatch ->
                        success (text, reverse results)
                    Error error errors ->
                        failure $ Error error errors
            Right (text', result) ->
                parse (result : results) text'

repeat1 ::
    Parser error result ->
    Parser error (result, [result])
repeat1 parser =
    do
        result <- parser
        results <- repeat parser
        pure (result, results)

optional ::
    forall error result.
    Parser error result ->
    Parser error (Maybe result)
optional (Parser parseValue) =
    Parser parse
  where
    parse ::
        [Char] ->
        Either (Failure error) ([Char], Maybe result)
    parse text =
        case parseValue text of
            Left failure_ ->
                case failure_ of
                    NoMatch ->
                        success (text, Nothing)
                    Error error errors ->
                        failure $ Error error errors
            Right (text', result) ->
                success (text', Just result)

commit ::
    forall error result.
    error ->
    Parser error result ->
    Parser error result
commit error (Parser parseValue) =
    Parser parse
  where
    parse ::
        [Char] ->
        Either (Failure error) ([Char], result)
    parse text =
        case parseValue text of
            Left NoMatch ->
                failure $ Error error []
            Left (Error error' errors) ->
                failure $ Error error (error' : errors)
            Right (text', result) ->
                success (text', result)
