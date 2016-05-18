module Cipher where

import Data.Char

shiftRight :: Int -> Char -> Char
shiftRight i = chr . constrainToLetters . (+i) . ord

shiftLeft :: Int -> Char -> Char
shiftLeft i = chr . constrainToLetters . (i-) . ord

-- ord 'a' == 97; ord 'z' == 122
constrainToLetters :: Int -> Int
constrainToLetters x
 | x >= 97 && x <= 122 = x
 | x > 122 = constrainToLetters ((mod x 122) + 96)
 | x < 97 = constrainToLetters (97 - x + 96)
 | otherwise = x

ceasarCipher :: String -> Int -> String
ceasarCipher s i = map (shiftRight i) s

unCeasarCipher :: String -> Int -> String
unCeasarCipher s i = map (shiftLeft i) s
