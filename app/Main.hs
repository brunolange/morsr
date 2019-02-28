module Main where

import Morsr
import Data.Char

p :: String -> String
p code = case decode code of
    Just msg -> msg
    Nothing -> ""

main :: IO ()
main = interact $ unlines . map p . lines
