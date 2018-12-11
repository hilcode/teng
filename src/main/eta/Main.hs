{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTSyntax        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader
import Data.Eq (Eq)
import Data.Foldable (concat)
import Data.Function (($))
import Data.Functor (fmap)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as DataMap
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.Text (Text)
import qualified Data.Text as DataText
import Data.Text.IO as DataTextIO
import System.IO (IO)
import Text.Show (show)

import Prelude (Bool, Int, error, undefined)

main :: IO ()
main = DataTextIO.putStr $ DataText.concat $ templateMapToText localEnvironment templateThree "'" "'\n"

class IsMap map key value where
    (!) :: Ord key => map -> key -> value

instance IsMap (DataMap.Map key value) key value where
    (!) = (DataMap.!)

data AvailableTemplates
    = AvailableTemplates (DataMap.Map TemplateName Template)

instance IsMap AvailableTemplates TemplateName Template where
    (!) (AvailableTemplates map) templateName = map ! templateName

class HasAvailableTemplates a where
    get_available_templates :: a -> AvailableTemplates

class HasScope a where
    get_scope :: a -> Scope

data Environment
    = Environment AvailableTemplates Scope

instance HasAvailableTemplates Environment where
    get_available_templates (Environment availableTemplates _) = availableTemplates

instance HasScope Environment where
    get_scope (Environment _ scope) = scope

localEnvironment :: Environment
localEnvironment = Environment (AvailableTemplates (insertTemplates [templateOne, templateTwo, templateThree] DataMap.empty)) (Scope DataMap.empty)

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

templateThreeName :: TemplateName
templateThreeName
    = TemplateName "template-three"

templateThree :: Template
templateThree
    = Template
       templateThreeName
       [ SimpleTemplateLine [ Verbatim "-----" ]
       , TemplateInvocation [ Verbatim "    " ] templateTwoName []
       , SimpleTemplateLine [ Verbatim "=====" ]
       ]

data Value
    = IntValue Int
    | TextValue Text
    | BoolValue Bool
    | ListValue [Value]
    | MapValue (Map Text Value)

data Variable
    = IntVariable VariableName Value
    | TextVariable VariableName Value
    | BoolVariable VariableName Value
    | ListVariable VariableName Value
    | MapVariable VariableName Value
    | AdtVariable VariableName (Map Text Variable)

data Scope
    = Scope (Map VariableName Variable)

extractTemplate :: HasAvailableTemplates a => a -> TemplateName -> Template
extractTemplate env templateName
    = (get_available_templates env) ! templateName

class ToText environment a where
    toText :: HasAvailableTemplates environment => HasScope environment => environment -> a -> Text

class ToTextList environment a where
    toTextList :: HasAvailableTemplates environment => HasScope environment => environment -> a -> [Text]

class HasTemplateName a where
    name :: a -> TemplateName

newtype TemplateName
    = TemplateName Text
      deriving
        (Eq, Ord)

newtype VariableName
    = VariableName Text

data TemplateLineElement
    = Verbatim Text
    | Dynamic Variable

instance ToText env Value where
    toText _ (IntValue intValue)
        = DataText.pack (show intValue)
    toText _ (BoolValue boolValue)
        = DataText.pack (show boolValue)
    toText _ (TextValue textValue)
        = textValue
    toText environment (ListValue values)
        = "[" <> foldl' (<>) "" ((toText environment :: Value -> Text) `fmap` values) <> "]"
    toText environment (MapValue map)
        = "{" <> (foldl' (<>) "" valuesAsText) <> "}"
          where
            keys = DataMap.keys map
            toValue :: IsMap (Map Text Value) Text Value => Text -> Value
            toValue = (!) map
            values = toValue `fmap` keys
            valuesAsText = (toText environment) `fmap` values

instance ToText env (Map Text Value) where
    toText environment map
        = "{" <> DataText.intercalate ", " keyValues <> "}"
          where
            keys = DataMap.keys map
            toValue :: IsMap (Map Text Value) Text Value => Text -> Value
            toValue = (!) map
            keyValues :: [Text]
            keyValues = [(key <> " -> " <> toText environment (toValue key)) | key <- keys]

instance ToText env Variable where
    toText environment (IntVariable _ value@(IntValue _))
        = toText environment value
    toText _ (IntVariable _ _)
        = error "This should never happen: an IntVariable must have an IntValue."
    toText environment (BoolVariable _ value@(BoolValue _))
        = toText environment value
    toText _ (BoolVariable _ _)
        = error "This should never happen: a BoolVariable must have a BoolValue."
    toText environment (TextVariable _ value@(TextValue _))
        = toText environment value
    toText _ (TextVariable _ _)
        = error "This should never happen: a TextVariable must have a TextValue."
    toText environment (ListVariable _ value@(ListValue _))
        = toText environment value
    toText _ (ListVariable _ _)
        = error "This should never happen: a ListVariable must have a ListValue."
    toText environment (MapVariable _ value@(MapValue _))
        = toText environment value
    toText _ (MapVariable _ _)
        = error "This should never happen: a MapVariable must have a MapValue."
    toText environment (AdtVariable _ map)
        = toText environment map

instance ToText env (Map Text Variable) where
    toText environment map
        = "{" <> DataText.intercalate ", " keyValues <> "}"
          where
            keys = DataMap.keys map
            toVariable :: IsMap (Map Text Variable) Text Variable => Text -> Variable
            toVariable = (!) map
            keyValues :: [Text]
            keyValues = [(key <> " -> " <> toText environment (toVariable key)) | key <- keys]

instance ToText env TemplateLineElement where
    toText _ (Verbatim text)
        = text
    toText environment (Dynamic variable)
        = toText environment variable

instance ToText env [TemplateLineElement] where
    toText environment templateLineElements
        = foldl' (<>) "" $ (toText environment) `fmap` templateLineElements


data TemplateLine
    = SimpleTemplateLine ![TemplateLineElement]
    | TemplateInvocation ![TemplateLineElement] !TemplateName ![TemplateLineElement]

instance ToTextList env TemplateLine where
    toTextList environment (SimpleTemplateLine templateLineElements)
        = [toText environment templateLineElements]
    toTextList environment (TemplateInvocation prefixTemplateLineElements templateName suffixTemplateLineElements)
        = templateMapToText environment (extractTemplate environment templateName) prefix suffix
          where
            prefix = toText environment prefixTemplateLineElements
            suffix = toText environment suffixTemplateLineElements

instance ToTextList env [TemplateLine] where
    toTextList environment templateLines
        = concat $ (toTextList environment) `fmap` templateLines


data Template
    = Template !TemplateName ![TemplateLine]

instance HasTemplateName Template where
    name (Template templateName _)
        = templateName

templateMapToText :: HasAvailableTemplates environment => HasScope environment => environment -> Template -> Text -> Text -> [Text]
templateMapToText environment (Template _ templateLines) prefix suffix
    = (\text -> prefix <> text <> suffix) `fmap` lines
      where
        lines = toTextList environment templateLines
