module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord x
  | x == 1 = "one"
 |  x == 2 = "two"
 |  x == 3 = "three"
 |  x == 4 = "four"
 |  x == 5 = "five"
 |  x == 6 = "six"
 |  x == 7 = "seven"
 |  x == 8 = "eight"
 |  x == 9 = "nine"
 | otherwise = "unknown"

digits :: Int -> [Int]
digits x
 | x == 0 = []
 | otherwise = digits (div x 10) ++ [mod x 10]

-- Split into digits, turn into words 
-- Add "+" between each word
-- Concat each string in the list to make one string
wordNumber :: Int -> String
wordNumber x = concat $ intersperse "+" $ map digitToWord $ digits x
