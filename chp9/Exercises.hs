module Exercises where

import Data.Char
import Data.Bool

instance Num Bool where
    (+) False 1 = True
    (+) _ _ = undefined
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger 0 = False
    fromInteger 1 = True
    fromInteger _ = undefined

eftBool :: Bool -> Bool -> [Bool]
eftBool start end
 | start == end = [end]
 | start > end = []
 | otherwise = start : eftBool (start+1) end

instance Num Ordering where
    (+) LT 1 = EQ
    (+) EQ 1 = GT
    (+) _ _ = undefined
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger 0 = LT
    fromInteger 1 = EQ
    fromInteger 2 = GT
    fromInteger _ = undefined

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
eftChar start end
 | start == end = [end]
 | start > end = []
 | otherwise = start : eftChar (incChar start) end
 where incChar = chr . (+1) . ord


-- Split string by space
-- Take the first element up to space
-- Repeat function with parameter: Drop until finding a space and then drop up to space
myWords :: String -> [String]
myWords s
 | s == "" = []
 | otherwise = takeWhile (/=' ') s : myWords (dropWhile (== ' ') $ dropWhile (/=' ') s)

myLines :: String -> [String]
myLines s
 | s == "" = []
 | otherwise = takeWhile (/='\n') s : myWords (dropWhile (== '\n') $ dropWhile (/='\n') s)

mySplitString :: Char -> String -> [String]
mySplitString split str
 | str == "" = []
 | otherwise = takeWhile (/=split) str : mySplitString split (dropWhile (== split) $ dropWhile (/=split) str)

myWords' = mySplitString ' '
myLines' = mySplitString '\n'

-- List Comprehensions (like Python!)
listC1 = [ x^y | x <- [1..10], y <- [1,2], x^y < 20]
listC2 = [ x | x <- [1..10], y <- [1,2], x^y < 20] -- The predicate determines if there is an output

-- Remove all lower case letters from a string
removeLowerCase str = [x | x <- str, elem x ['A'..'Z']]

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

myTuples = length [(x,y) | x <- mySqr, y <- myCube, x < 50 && y < 50]

-- Bottom Madness
-----------------
-- Will this "bottom"? - (Not return a result)
bottom1 = [x^y | x <- [1..5], y <- [2, undefined]] -- bottoms!
bottom2 = take 1 bottom1 -- [1]

-- Will this return a value?
b1 = sum[1,undefined,3] -- no
b2 = length [1,2, undefined] -- yes, result is 3
b3 = length $ [1,2,3] ++ undefined -- no, because the spine itself is undefined?
b4 = take 1 $ filter even [1,2,3,undefined] -- yes, result is [2]
b5 = take 1 $ filter even [1,3,undefined] -- no since evaluation will reach undefined element
b6 = take 1 $ filter odd [1,3,undefined] -- yes, result is [1]
b7 = take 2 $ filter odd [1,3,undefined] -- yes, result is [1,3]
b8 = take 3 $ filter odd [1,3,undefined] -- no since evaluation will reach undefined element

-- Weak Head Normal Form or Normal Form or neither?
-- By ‘normal form’ we mean that the expression is fully evaluated. 
-- Weak head normal form’ means the expression is only evaluated as far as is necessary to reach a data constructor.
b9 = [1,2,3,4,5] -- WHNF and NF
-- b10 = 1:2:3:4:_ -- Neither WHNF or NF??  "Found hole '_' with type [a]
b10 = enumFromTo 1 10 -- WHNF since it can be evaluated
b11 = length [1,2,3,4,5] -- WHNF since it can be evaluated 
b12 = sum (enumFromTo 1 10) -- WHNF since it can be evaluated 
b13 =  ['a'..'m'] ++ ['n' .. 'z'] -- WHNF since it can be evaluated 
-- b14 = (_,'b') -- WHNF since it can be evaluated 

-- More Bottoms
-- Return a value or bottom?
b15 = take 1 $ map (+1) [undefined, 2, 3] -- bottom
b16 = take 1 $ map (+1) [1, undefined, 3] -- return value
b17 = take 2 $ map (+1) [1, undefined, 3] -- bottom
itIsMystery xs = map (\x -> elem x "aeiou") xs -- Returns true if character is a vowel

usingIfThenElse = map (\x -> if x == 3 then (-x) else (x)) [1..10]
usingBool = map (\x->bool x (-x) (x==3)) [1..10]
usingBool' = map (\x->bool x (-x) (x==3))

-- Filtering
filter3 = filter (\x-> (rem x 3) == 0)
howManyMultiples3 = length . filter3

-- Remove all articles (’the’, ’a’, and ’an’) from sentences
removeArticles :: String -> [String]
removeArticles = filter (\x-> not $ elem x ["the", "a", "an"]) . words

-- My version of zip
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

myZip' = myZipWith (\x y->(x,y))

-- Chapter Exercises
allUpperCase = filter (isUpper)
capitalizeFirst (x:xs) = toUpper x : xs 

capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs 

capitalizeHead = toUpper . head


-- My own standard functions
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) 
 | x = x
 | otherwise = myOr xs

myAny :: (a->Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
 | f x = True
 | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem toMatch (x:xs)
 | toMatch == x = True
 | otherwise = myElem toMatch xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' toMatch = any (==toMatch)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse x = last x : myReverse (init x)

squish :: [[a]] -> [a]
squish = foldl1 (++)

squishMap :: (a->[b]) -> [a] -> [b]
squishMap f x = squish $ map f x

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- myMaximumBy (\_ _ -> GT) [1..10]  == 1
-- Traverses the array like a foldl
-- Since the first number (1) compared to any number
-- always returns GT, 1 is the final answer
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl1 (findOrdering GT f)

findOrdering :: Ordering -> (a -> a -> Ordering) -> a -> a -> a
findOrdering o f x y
 | f x y == o = x
 | otherwise = y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl1 (findOrdering LT f)

myMaximum :: (Ord a, Foldable t) -> t a -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a, Foldable t) -> t a -> a
myMinimum = myMinimumBy compare
