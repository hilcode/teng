module Template (Eol(..), Line(..), Template(..), parse, toText) where

import           Control.Applicative
    ((<|>))
import qualified Data.Foldable        as Foldable
import           Data.Sequence
    (Seq)
import qualified Data.Sequence        as Sequence
import           Data.Text
    (Text)
import qualified Data.Text            as Text
import           Data.Void
    (Void)
import           Prelude              hiding
    (lines)
import           Text.Megaparsec
    (Parsec)
import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MPC

data Eol = LF | CR_LF | EOF deriving (Show, Eq)

data Line
    = Line Text Eol
    deriving (Show, Eq)

extractContent :: Line -> Text
extractContent (Line x _) = x

data Template
    = Template (Seq Line)
    deriving (Show, Eq)

data Value
    = ValueInt Int
    | ValueBoolean Bool
    | ValueText Text

class ToText a where
    toText :: a -> Text

instance ToText Value where
    toText (ValueInt int)       = Text.pack (show int)
    toText (ValueBoolean True)  = "true"
    toText (ValueBoolean False) = "false"
    toText (ValueText text)     = text

data Argument
    = Argument Text Value

type Parser = Parsec Void Text

parseEol :: Parser Eol
parseEol = parseEol' <|> parseEof'
  where
    parseEol' :: Parser Eol
    parseEol' = do
        eol <- MPC.eol
        return $ case eol of
            "\r\n" -> CR_LF
            _      -> LF
    parseEof' :: Parser Eol
    parseEof' = do
        MP.eof
        return EOF

parseDivider :: Parser ()
parseDivider = do
    MPC.string "-----" <* parseEol
    return ()

parseLine :: Parser Line
parseLine = do
    parseNonEmptyLine <|> parseEmptyLine
      where
        parseNonEmptyLine :: Parser Line
        parseNonEmptyLine = do
            text <- MP.takeWhile1P Nothing (\ch -> ch /= '\r' && ch /= '\n')
            eol <- parseEol
            return $ Line text eol
        parseEmptyLine :: Parser Line
        parseEmptyLine = do
            eol <- MPC.eol
            case eol of
                "\r\n" -> return $ Line "" CR_LF
                _      -> return $ Line "" LF

parseLines :: Parser (Seq Line)
parseLines = do
    lines <- MP.many parseLine
    return $ Sequence.fromList lines

parseTemplate :: Parser Template
parseTemplate = do
    parseDivider
    lines <- parseLines
    MP.eof
    return $ Template lines

parse :: Text -> Template
parse source = case MP.parse parseTemplate "" source of
    Left e       -> error $ show e
    Right result -> result

surround :: Text -> Text -> Text -> Text
surround prefix suffix text = prefix <> text <> suffix

instance ToText Template where
    toText (Template lines) = "Template" <> Text.concat (Foldable.toList $ surround "\n\t'" "'" <$> extractContent <$> lines)
