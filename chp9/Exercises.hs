module Exercises where

eftBool :: Bool -> Bool -> [Bool]
eftBool x y = [x,y]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = [x,y]

eftInt :: Int -> Int -> [Int]
eftInt start end
 | start == end = [end]
 | start > end = []
 | otherwise = start : eftInt (start+1) end

eftChar :: Char -> Char -> [Char]
eftChar = undefined
