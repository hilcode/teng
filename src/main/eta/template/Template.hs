module Template (Template(..), parse, toText) where

import           Control.Applicative
    ((<|>))
import           Data.Text
    (Text)
import qualified Data.Text            as Text
import           Data.Void
    (Void)
import           Text.Megaparsec
    (Parsec)
import qualified Text.Megaparsec      as MP
import qualified Text.Megaparsec.Char as MPC

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

parseEol :: Parser ()
parseEol = do
    MPC.eol
    return ()

parseDivider :: Parser ()
parseDivider = do
    _ <- MPC.string "-----" <* (parseEol <|> MP.eof)
    return ()

parseTemplate :: Parser Template
parseTemplate = do
    _ <- parseDivider
    return $ Template ""

parse :: Text -> Template
parse source = case MP.parse parseTemplate "" source of
    Left e       -> error $ show e
    Right result -> result

data Template = Template Text deriving (Show, Eq)

instance ToText Template where
    toText (Template text) = "Template '" `Text.append` text `Text.append` "'"
