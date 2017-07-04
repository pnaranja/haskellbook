module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

main :: IO ()
main = quickCheck prop_thereAndBackAgain

allowedChars :: String
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

-- To test, run in repl: sample' $ elements $ M.elems letterToMorse

-- Verify when we convert something to Morse code and then back again, 
-- we get the same value we started with
prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = 
    forAll charGen
    (\c -> (charToMorse c >>= morseToChar) == Just c)
