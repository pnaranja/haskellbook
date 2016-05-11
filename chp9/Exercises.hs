module Exercises where

instance Num Bool where
    (+) False 1 = True
    fromInteger 0 = False
    fromInteger 1 = True

eftBool :: Bool -> Bool -> [Bool]
eftBool start end
 | start == end = [end]
 | start > end = []
 | otherwise = start : eftBool (start+1) end

instance Num Ordering where
    (+) LT 1 = EQ
    (+) EQ 1 = GT
    fromInteger 0 = LT
    fromInteger 1 = EQ
    fromInteger 2 = GT

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start end
 | start == end = [end]
 | start > end = []
 | otherwise = start : eftOrd (start+1) end 

eftInt :: Int -> Int -> [Int]
eftInt start end
 | start == end = [end]
 | start > end = []
 | otherwise = start : eftInt (start+1) end

eftChar :: Char -> Char -> [Char]
eftChar = undefined
