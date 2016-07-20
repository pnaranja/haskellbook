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
row2 = Jam Peach 2
row3 = Jam Peach 3
row4 = Jam Peach 4
row5 = Jam Peach 5
row6 = Jam Peach 6
allJam = [row1,row2,row3,row4,row5,row6]

totalJars = sum (map amt allJam)
-- Created max instance for this to work
mostRow = maximum allJam
