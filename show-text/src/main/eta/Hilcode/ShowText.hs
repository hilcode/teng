module Hilcode.ShowText (ShowText(showT)) where

import Data.Text (Text)

class ShowText a where
    showT :: a -> Text
