module Hilcode.FieldName.Internal where

import Data.Text ( Text )
import Hedgehog ( Gen )
import Hedgehog.Gen ( latin1, text )
import Hedgehog.Range ( linear )

newtype FieldName
    = FieldName Text
        deriving (Show, Eq, Ord)

generate :: Gen FieldName
generate = FieldName <$> text (linear 1 20) latin1
