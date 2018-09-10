module Main where

import qualified Data.Text.IO as TextIO
import qualified Template as Template

main :: IO ()
main = do
    TextIO.putStrLn $ Template.toText template
      where
        template = Template.parse "-----\nhello\nworld\n!"
