module Hilcode.Key (
    Key (..),
) where

newtype Key
    = Key String
    deriving (Eq, Ord)

instance Show Key where
    show (Key key) =
        '"' : key ++ ['"']
