module Main where

import Morsr
import Data.Char

process :: String -> String
process code = case decode code of
    Just msg -> msg
    Nothing -> ""

main :: IO ()
main = interact $ unlines . map process . lines
