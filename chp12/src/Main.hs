module Main where

import Control.Applicative
import Data.Maybe
import Data.List

main :: IO ()
main = putStrLn "Start chp12"

-- Smart Constructors
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
-- But we can make an empty string for Name and a negative number for Age!
-- Lets constrain this with Maybe
-- mkPerson is considered a "Smart Constructor"
mkPerson :: Name -> Age -> Maybe Person
mkPerson n a
    | n /= "" && a >= 0 = Just $ Person n a
    | otherwise = Nothing

-- Use the Either datatype to tell the user what exactly is wrong when trying to construct Person
-- Left is our "Error Constructor"
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq,Show)

mkPerson2 :: Name -> Age -> Either PersonInvalid Person
mkPerson2 n a
    | n == "" = Left NameEmpty
    | a <= 0 = Left AgeTooLow
    | otherwise = Right $ Person n a

-- Notice that if Name AND AGE are invalid, it will only show NameEmpty!
-- Lets fix this and show the user both errors!  Make seperate checking and combine results
type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
    | age >= 0 = Right age
    | otherwise = Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay n
  | n /= "" = Right n
  | otherwise = Left [NameEmpty]

mkPerson3 :: Name -> Age -> ValidatePerson Person
mkPerson3 n a = mkPerson3' (nameOkay n) (ageOkay a)

mkPerson3' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson3' (Right n) (Right a) = Right (Person n a)
mkPerson3' (Left [NameEmpty]) (Right a) = Left [NameEmpty]
mkPerson3' (Right n) (Left [AgeTooLow]) = Left [AgeTooLow]
mkPerson3' (Left [NameEmpty]) (Left [AgeTooLow]) = Left [NameEmpty,AgeTooLow]

-- Later on, learn that we can replace this with...
mkPerson4 :: Name -> Age -> ValidatePerson Person
mkPerson4 n a = liftA2 Person (nameOkay n) (ageOkay a)
-- where liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c


-- Kinds
-- Type Constructors (Higher Kinded types) are types that take more types as arguments
data Example a = Blah | RoofGoats | Woot a deriving Show
-- :k Example --> Example :: * -> *
-- Example must be applied to one type in order to become a concrete type represented by (*)
--
-- (,) is a two-tuple
-- :k (,) --> (,) :: * -> * -> *
-- :k (Int,Int) --> (Int,Int) :: *
--
-- Either is the same - needs two arguments
-- :k Either --> Either :: * -> * -> *
-- :k Either Int String --> Either Int String :: *
--
-- [] is also higher kinded
-- :k [] --> [] :: * -> *
-- :k [Int] --> [Int] :: *
--
-- Maybe is also higher kinded
-- :k Maybe --> Maybe :: * -> *
--
-- Maybe [] will not work but Maybe [Int] will work
--
-- Also note:  The Arrow (->) refers to a need for application!
-- Once the arrows are gone then no need to apply anything for a concrete type
-- :k Maybe Char --> Maybe Char :: *

--Safe version of tail:
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (x:xs) = Just xs
--
--Safe version of head:
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Data Constructors are functions!
data Paul = Paul Int deriving Show
-- :t Paul --> Paul :: Int -> Paul

-- Have polymorphic types
-- Usually need to add the variable in the type constructor
data Paul2 a = Paul2 a deriving Show
-- :t Paul2 --> Paul2 :: a -> Paul2
-- Paul2 4 = Paul2 4
-- Paul2 "Naranja" = Paul2 "Naranja"
-- Paul2 <$> [1,2,3] = [Paul2 1, Paul2 2, Paul2 3]
-- Paul2 <$> "Hi" = [Paul2 H, Paul2 i]

-- 12.5 - String Processing
-- Break up String and replace "the" with "a"
replaceThe :: String -> String
replaceThe str = unwords $ (nothingToA . notThe) <$> words str

nothingToA :: Maybe String -> String
nothingToA Nothing = "a"
nothingToA (Just x) = x

-- Nothing the string is "the", else Just a
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a


-- Count the number of instances of "the" followed by a vowel-initial word
countBeforeTheVowel :: String -> Int
countBeforeTheVowel = countHeadAndVowel 0 . words

countHeadAndVowel :: Int -> [String] -> Int
countHeadAndVowel acc [] = acc
countHeadAndVowel acc ([a]) = acc
countHeadAndVowel acc (a:b:c) = 
    if (isNothing $ notThe a) && (hasHeadVowel b) then (countHeadAndVowel (acc+1) c) else (countHeadAndVowel acc (b:c))

hasHeadVowel :: String -> Bool
hasHeadVowel str = fromMaybe False $ isVowel <$> safeHead str

isVowel :: Char -> Bool
isVowel x
    | x=='a' || x=='e' || x=='i' || x=='o' || x=='u' = True
    | otherwise = False


-- Return the number of letters that are vowels in a word
countVowels :: String -> Int
countVowels str = length $ filter id $ isVowel <$> str


-- Write a function that counts the number of vowels in a string and the number of consonants. 
-- If the number of vowels exceeds the number of consonants, the function returns Nothing.
newtype Word' = Word' String deriving (Show, Eq)

countConsonants str = length $ filter not $ isVowel <$> str

mkWord :: String -> Maybe Word'
mkWord str = if countVowels str > countConsonants str then Nothing else Just (Word' str)

-- Natural numbers
data Nat = Zero | Succ Nat deriving (Eq,Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i 
    | i<0 = Nothing
    | otherwise = Just (integerToNat' i)

integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' 1 = Succ Zero
integerToNat' i = Succ (integerToNat' (i-1))


-- Small Library for Maybe
isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' x = True

isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' a = False

mayybee :: Num b => b -> (a->b) -> Maybe a -> b
mayybee x f m = fromMaybe x $ f <$> m

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x Nothing = x
fromMaybe' _ (Just y) = y

listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' arr = safeHead arr

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just a) = [a]

catMaybes' :: [Maybe a] -> [a]
catMaybes' a = fromJust <$> filter isJust a

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe a
    | any isNothing' a = Nothing
    | otherwise = Just (catMaybes' a)


-- Small Library for Either
isLeft :: Either a b -> Bool
isLeft (Left a) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right b) = True
isRight _ = False

-- UNSAFE! --
fromLeft :: Either a b -> a
fromLeft (Left a) = a

fromRight :: Either a b -> b
fromRight (Right b) = b
--------------

lefts' :: [Either a b] -> [a]
lefts' a = fromLeft <$> filter isLeft a

lefts'' :: [Either a b] -> [a]
lefts'' [] = []
lefts'' (x:xs)
    | isLeft x = fromLeft x : lefts'' xs
    | not $ isLeft x = lefts'' xs
    | otherwise = []

rights'' :: [Either a b] -> [b]
rights'' [] = []
rights'' (x:xs)
    | isRight x = fromRight x : rights'' xs
    | not $ isRight x = rights'' xs
    | otherwise = []

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' x = (lefts'' x, rights'' x)

-- Tried Data.Bifunctor.bimap but types don't match!
eitherMaybe' :: (b->c) -> Either a b -> Maybe c
eitherMaybe' f e = if isRight e then Just (f $ fromRight e) else Nothing
--eitherMaybe' f e = if isRight e then Just (fromRight $ bimap f f e) else Nothing


-- Anamorphisms! -> Building up data structures!

myIterate :: (a->a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x = case f x of
                 Just (a,b) -> a : myUnfoldr f b
                 Nothing -> []

betterIterate :: (a->a) -> a -> [a]
betterIterate f = myUnfoldr (\x->Just (x,f x))
