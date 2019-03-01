module Morsr
    ( decode
    ) where

import Data.List.Split

data Morse = Dot | Dash | Space deriving (Eq, Show)

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

morseTree :: Tree Char
morseTree =
    Node '*'
        (Node 'E'
            (Node 'I'
                (Node 'S'
                    (Node 'H'
                        (Node '5' Empty Empty)
                        (Node '4' Empty Empty)
                    )
                    (Node 'V'
                        Empty
                        (Node '3' Empty Empty)
                    )
                )
                (Node 'U'
                    (Node 'F' Empty Empty)
                    (Node '|'
                        Empty
                        (Node '2' Empty Empty)
                    )
                )
            )
            (Node 'A'
                (Node 'R'
                    (Node 'L' Empty Empty)
                    Empty
                )
                (Node 'W'
                    (Node 'P' Empty Empty)
                    (Node 'J'
                        Empty
                        (Node '1' Empty Empty)
                    )
                )
            )
        )
        (Node 'T'
            (Node 'N'
                (Node 'D'
                    (Node 'B'
                        (Node '6' Empty Empty)
                        Empty
                    )
                    (Node 'X' Empty Empty)
                )
                (Node 'K'
                    (Node 'C' Empty Empty)
                    (Node 'Y' Empty Empty)
                )
            )
            (Node 'M'
                (Node 'G'
                    (Node 'Z'
                        (Node '7' Empty Empty)
                        Empty
                    )
                    (Node 'Q' Empty Empty)
                )
                (Node 'O'
                    (Node '|'
                        (Node '8' Empty Empty)
                        Empty
                    )
                    (Node '|'
                        (Node '9' Empty Empty)
                        (Node '0' Empty Empty)
                    )
                )
            )
        )

data Direction = L | R deriving (Show)

type Path = [Direction]

navigate :: Tree Char -> Path -> Char
navigate (Node '|' _ _) [] = error "invalid character"
navigate (Node x _ _) [] = x
navigate (Node _ l _) (L:xs) = navigate l xs
navigate (Node _ _ r) (R:xs) = navigate r xs
navigate Empty xs = error "invalid message"

morseWords :: [Morse] -> [[Morse]]
morseWords = splitWhen (== Space)

morseToDirection :: Morse -> Direction
morseToDirection Dot = L
morseToDirection Dash = R

decode' :: [Morse] -> String
decode' = map (navigate morseTree) . map (map morseToDirection) . morseWords

charToMorse :: Char -> Maybe Morse
charToMorse '.' = Just Dot
charToMorse '-' = Just Dash
charToMorse ' ' = Just Space
charToMorse _   = Nothing

decode :: String -> Maybe String
decode = fmap decode' . sequence . map charToMorse
