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

-- myFoldr (-) 1 [3,4] == 3-(4-1) == 0
--
-- If f doesn't evaluate the second arg "(myFoldr f acc xs)"
-- it can avoid the lists values and spine as well
-- let u = undefined
-- foldr (+) 0 (take 4 ([1,2,3,4] ++ u)) == 10
-- foldr (+) 0 (take 2 (take 10 ([1,2,3,4] ++ u))) == 3  //Doesn't matter if take 10 bottoms!
-- foldr (\_ _ -> 9001) 0 [undefined, undefined] == 9001 !!
-- foldr const 0 [1..3] == const 1 (const 2 (const 3 0)) == const 1 _ == 1
-- foldr const 0 [undefined, 2] == const undefined (const 2 0) == const undefined _ = Exception

myFoldl :: (b->a->b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- myFoldl (-) 1 [3,4] == (1-3)-4 == -6
-- Scans list all the intermediate steps of a fold
-- scanl (-) 1 [3,4] == [1,-2,-6]
--
-- last (scanl f z xs) == foldl f z xs
-- head (scanr f z xs) == foldr f z xs

-- Folds as substitutions
-- [1..3]           == 1 : 2 : 3 : []
-- foldr f z [1..3] == 1 'f' (2 'f' (3 'f' z))
--
-- But not really with fold left
-- foldl f z [1..3] == (((z 'f' 1) 'f' 2) 'f' 3)
--
-- foldl (flip (:)) [] [1..3] == ((([] ('flip' (:)) 1) ('flip' (:)) 2) ('flip' (:)) 3)
--  == (((3 : 2) : 1) : []) == [3,2,1]
--
--
-- Exercises: Understanding Folds
-- 1. foldr (*) 1 [1..5] == foldl (*) 1 [1..5]
-- 2. foldl (flip (*)) 1 [1..3] == (((3 * 2) * 1) *1)
-- 3. foldr, but not foldl, associates to the right
-- 4. Catamorphisms reduce structure
-- 5. a) foldr (++) "WOOT" ["woot", "WOOT", "woot"]
--    b) foldr max 'z' "fear is the little death" == 'z'
--    c) foldr (:) [True] [False, True] == [False,True,True]
--    d) Always returns True?
--    e) foldr ((++) . show) "" [1..5]
--    f) foldl const 'a' [1..5] -- foldr in this case, will require type to be a Char (type of 'a')
--    g) foldl const 0 "tacos" -- same reason as problem f
--    h) foldr (flip const) 0 "bur" 
--    i) foldr (flip const) 'z' [1..5]

-- Take the first three letters of each String value in a list of strings concatenate the result
pab =["Pizza", "Apple", "Bannana"]
takeFirstLetters :: [String] -> String
takeFirstLetters = foldl (\x y-> x ++ (take 3 y)) ""

takeFirstLetters' :: [String] -> String
takeFirstLetters' = foldr (\y x-> x ++ (take 3 y)) ""


-- Scan Exercises
fibs' = 1 : scanl (+) 1 fibs'
fibs = take 20 $ fibs'

fibs2 = filter (<100) fibs

-- Factorial using scan
-- Remember 0 factorial is 1!
myFactorial :: Integer -> Integer
myFactorial 0 = 1
myFactorial x = last $ scanl (*) 1 [1..x]


-- Warmup and Review

stops = "pbtdkg"
vowels = "aeiou"

stopsAndVowels :: [(Char,Char,Char)]
stopsAndVowels = [ (x1,v1,x2) | x1 <- stops, x2 <- stops, v1 <-vowels, x1/=x2 ]

withP :: (Char,Char,Char) -> Bool
withP (_, _, 'p') = True
withP ('p', _, _) = True
withP _ = False

stopsAndVowelsWithP :: [(Char,Char,Char)]
stopsAndVowelsWithP = filter withP stopsAndVowels

nouns = ["man", "woman", "car", "truck", "boat", "bathroom", "classroom", "computer"]
verbs = ["eats", "walks", "jumps", "runs", "breaks", "punches"]

nounsAndVerbs :: [(String,String,String)]
nounsAndVerbs = [ (x1,v1,x2) | x1 <- nouns, x2 <- nouns, v1 <-verbs, x1/=x2 ]

-- seekritFunc is a function that divides the total letters of the string by the number of words in the string
-- Be more precise - use fractional division
seekritFunc' :: Fractional a => String -> a
seekritFunc' x = (fromInteger $ toInteger (sum $ map length $ words x)) / (fromInteger $ toInteger (length $ words x))

seekritFunc'' :: Fractional a => String -> a
seekritFunc'' x = foldr1 (/) (map (fromInteger . toInteger) [sum $ map length $ words x , length $ words x])

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- Return True if any eval of f to an element in the list is True
myAny :: (a -> Bool) -> [a] -> Bool
myAny f lst = myOr $ map f lst
myAny' f = myOr . map f
myAny'' f = (.) myOr (map f)
myAny''' = (myOr .) . map   -- Got this from https://blunt.herokuapp.com.  Not sure how this works??

myElem :: Eq a => a -> [a] -> Bool
myElem x lst = myAny (x==) lst
myElem' x = myAny (x==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a->b) -> [a] -> [b]
myMap f = foldr (\x y-> f x : y) []
myMap' :: (a->b) -> [a] -> [b]
myMap' = flip foldr [] . ((:) .)  -- Got this from https://blunt.herokuapp.com.  Not sure how this works??

myFilter :: (a->Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x:y else y) []

-- Flatten a list of lists
squish :: [[a]] -> [a]
squish = foldr (\a b -> (foldr (:) [] a) ++ b) [] 

-- Map function over list and concatenates the result
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f
squishMap' = (squish .) . myMap  -- Got this from https://blunt.herokuapp.com.  Not sure how this works??

-- The squish function in terms of squishMap
squishAgain :: [[a]] -> [a]
squishAgain = undefined

-- Take comparison function and a list, return greatest element based on last value returned GT
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldr1 (\x y -> if f x y == GT then x else y)
--
-- Take comparison function and a list, return greatest element based on last value returned GT
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldr1 (\x y -> if f x y == LT then x else y)
