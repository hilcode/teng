module Hilcode.Misc
    ( (|>)
    , indexed
    )
where

(|>) :: a -> (a -> b) -> b
a |> f
    = f a

indexed :: [a] -> [(Int, a)]
indexed
    = go 0
  where
    go :: Int -> [a] -> [(Int, a)]
    go _ []
        = []
    go currentIndex (a:as)
        = (currentIndex, a) : go (currentIndex + 1) as
