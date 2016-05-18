module Cipher where

import Data.Char

shiftRight :: Int -> Char -> Char
shiftRight i = chr . constrainToLetters . (+i) . ord

-- ord 'a' == 97; ord 'z' == 122
constrainToLetters :: Int -> Int
constrainToLetters x
 | x >= 97 && x <= 122 = x
 | otherwise = constrainToLetters ((mod x 122) + 96)

ceasarCipher :: String -> Int -> String
ceasarCipher s i = map (\x-> shiftRight i x) s
