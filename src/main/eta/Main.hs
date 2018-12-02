{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad.Reader
import Data.Eq (Eq)
import Data.Foldable (concat)
import Data.Function (($))
import Data.Functor (fmap)
import Data.List (foldl')
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as DataMap
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.Text (Text)
import qualified Data.Text as DataText
import Data.Text.IO as DataTextIO
import System.IO (IO)

main :: IO ()
main = DataTextIO.putStr $ DataText.concat $ runReader (templateMapToText templateTwo "'" "'\n") localEnvironment

localEnvironment :: Environment
localEnvironment = Environment (insertTemplates [templateOne, templateTwo] DataMap.empty)

insertTemplate :: DataMap.Map TemplateName Template -> Template -> DataMap.Map TemplateName Template
insertTemplate map template
    = DataMap.insert (name template) template map

insertTemplates :: [Template] -> DataMap.Map TemplateName Template -> DataMap.Map TemplateName Template
insertTemplates templates map
    = foldl' insertTemplate map templates

templateOneName :: TemplateName
templateOneName
    = TemplateName "template-one"

templateOne :: Template
templateOne
    = Template
       templateOneName
       [ SimpleTemplateLine [ Verbatim "Hello", Verbatim " ", Verbatim "World!" ]
       , SimpleTemplateLine [ Verbatim "Goodbye", Verbatim " ", Verbatim "Cruel", Verbatim " ", Verbatim "World!" ]
       ]

templateTwoName :: TemplateName
templateTwoName
    = TemplateName "template-two"

templateTwo :: Template
templateTwo
    = Template
       templateTwoName
       [ SimpleTemplateLine [ Verbatim "First line" ]
       , TemplateInvocation [ Verbatim "    " ] templateOneName []
       , SimpleTemplateLine [ Verbatim "Last line" ]
       ]


data Environment
    = Environment
        (DataMap.Map TemplateName Template)

extractTemplate :: Environment -> TemplateName -> Template
extractTemplate (Environment map) templateName
    = map ! templateName


class ToText environment a where
    toText :: a -> Reader environment Text

class ToTextList environment a where
    toTextList :: a -> Reader environment [Text]

class HasName a where
    name :: a -> TemplateName

newtype TemplateName
    = TemplateName Text
      deriving
        (Eq, Ord)


data TemplateLineElement
    = Verbatim !Text

instance ToText Environment TemplateLineElement where
    toText (Verbatim text)
        = return text

instance ToText Environment [TemplateLineElement] where
    toText templateLineElements
        = do
            lineElements <- sequence $ toText `fmap` templateLineElements
            return $ foldl' (<>) "" lineElements


data TemplateLine
    = SimpleTemplateLine ![TemplateLineElement]
    | TemplateInvocation ![TemplateLineElement] !TemplateName ![TemplateLineElement]

instance ToTextList Environment TemplateLine where
    toTextList (SimpleTemplateLine templateLineElements)
        = sequence [toText templateLineElements]
    toTextList (TemplateInvocation prefixTemplateLineElements templateName suffixTemplateLineElements)
        = do
            prefix <- toText prefixTemplateLineElements
            suffix <- toText suffixTemplateLineElements
            environment <- ask
            templateMapToText (extractTemplate environment templateName) prefix suffix

instance ToTextList Environment [TemplateLine] where
    toTextList templateLines
        = do
            lines <- sequence $ toTextList `fmap` templateLines
            return $ concat $ lines


data Template
    = Template !TemplateName ![TemplateLine]

instance HasName Template where
    name (Template templateName _)
        = templateName

templateMapToText :: Template -> Text -> Text -> Reader Environment [Text]
templateMapToText (Template _ templateLines) prefix suffix
    = do
        lines <- toTextList templateLines
        return $ (\text -> prefix <> text <> suffix) `fmap` lines
