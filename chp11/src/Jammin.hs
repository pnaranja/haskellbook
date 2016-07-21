{-# LANGUAGE FlexibleInstances #-}

module Jammin where

import Data.List


data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show)
data JamJarsOld = JamOld Fruit Int deriving (Eq, Show)

data JamJars = Jam {fruit :: Fruit, amt :: Int} deriving (Eq,Show)
-- Cardinality of JamJars is (card of Int) * 4

instance Ord JamJars where
    (<) (Jam _ i1) (Jam _ i2) = i1 < i2
    (<=) (Jam _ i1) (Jam _ i2) = i1 <= i2
    (>) (Jam _ i1) (Jam _ i2) = i1 > i2
    (>=) (Jam _ i1) (Jam _ i2) = i1 >= i2
    max  (Jam f1 i1) (Jam f2 i2) = if i1>i2 then Jam f1 i1 else Jam f2 i2
    min  (Jam f1 i1) (Jam f2 i2) = if i1<i2 then Jam f1 i1 else Jam f2 i2


row1 = Jam Peach 1
row2 = Jam Apple 2
row3 = Jam Peach 2
row4 = Jam Blackberry 4
row5 = Jam Peach 5
row6 = Jam Plum 10
allJam = [row1,row2,row3,row4,row5,row6]

totalJars = sum (map amt allJam)
-- Created max instance for this to work
mostRow = maximum allJam

-- Sort allJams by their Fruit
-- Need to create instance of Ord Fruit
instance Ord Fruit where
    compare Peach Peach = EQ
    compare Peach Plum = LT
    compare Peach Apple = LT
    compare Peach Blackberry = LT
    compare Plum Plum = EQ
    compare Plum Peach = GT
    compare Plum Apple = LT
    compare Plum Blackberry = LT
    compare Apple Apple = EQ
    compare Apple Peach = GT
    compare Apple Plum = GT
    compare Apple Blackberry = LT
    compare Blackberry Blackberry = EQ
    compare Blackberry Peach = GT
    compare Blackberry Plum = GT
    compare Blackberry Apple = GT

compareKind (Jam k _) (Jam k' _) = compare k k'
sortJams = sortBy compareKind allJam
groupJam = groupBy (>) sortJams
