module Exercises where

mySum :: [Integer] -> Integer
mySum []      =  0
mySum (x:xs)  =  x + mySum xs

myLength :: [a] -> Integer
myLength []      =  0
myLength (_:xs)  =  1 + toInteger (myLength xs)

myProduct :: [Integer] -> Integer
myProduct []      =  1
myProduct (x:xs)  =  x * myProduct xs

myConcat :: [[a]] -> [a]
myConcat []      =  []
myConcat (x:xs)  =  x ++ myConcat xs

-- In each case, the base case is the IDENTITY for that function
-- Main function has a recursive pattern that associates to the right
-- Head of the list gets evaluated, set aside and then function moves to the right
-- Like.. foldr
myFoldr :: (a->b->b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- myFoldr (-) 1 [3,4] == 3-(4-1) = 0
--
-- If f doesn't evaluate the second arg "(myFoldr f acc xs)"
-- it can avoid the lists values and spine as well
-- let u = undefined
-- foldr (+) 0 (take 4 ([1,2,3,4] ++ u)) == 10
-- foldr (+) 0 (take 2 (take 10 ([1,2,3,4] ++ u))) == 3  //Doesn't matter if take 10 bottoms!
