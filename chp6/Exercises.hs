-- Exercises for Chapter 6
import Data.List (sort)

main :: IO ()
main = undefined

-- Intermission Exercises
data TisAnInteger = TisAnInteger Integer
instance Eq TisAnInteger where
    (==) (TisAnInteger a) (TisAnInteger a') = a == a'

data TwoIntegers = TwoIntegers Integer Integer
instance Eq TwoIntegers where
    (==) (TwoIntegers a b) (TwoIntegers a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = a == a'
    (==) (TisAString a) (TisAString a') = a == a'
    (==) _ _ = False

data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) (ThisOne a) (ThatOne a') = a == a'
    (==) (ThatOne a) (ThisOne a') = a == a'

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) (Hello _) (Goodbye _) = False
    (==) (Goodbye _) (Hello _) = False

--Chapter Exercises
-- TypeCheck
data Person = Person Bool
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
-- Should not type check because person is not an instance of Show
instance Show Person where
    show (Person a) = show a

data Mood = Blah | Woot deriving Show
settleDown x = if x == Woot
                  then Blah
                  else x
-- Should not type check because x can be infinite types, also no Eq type class instance of Mood
instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _ = False


type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
-- s1 is a partial function and is not instance of Show 


data Rocks = Rocks String deriving ( Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Probably won't type check because phew incorrectly forms Rocks and Yeah data constructors
--phew = Papu "chases" True

-- The truth function should type check because it uses proper data constructors for Rocks and Yeah
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- The equalityForAll function should type check because Papu is an instance of Eq
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p

-- The comparePapus function should not type check because Papu is not an instance of Ord
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p


-- Match the types
i :: a
i = undefined
-- Num a cannot turn into a

f :: Num a => a
f = undefined
-- Num a can turn into Fractional

f' :: Float
f' = undefined
-- Float can turn into Fractional

f'' :: Float
f'' = undefined
-- Float can turn into RealFrac

freud :: a -> a
freud = undefined
-- a->a can turn into Ord a => a -> a

freud' :: Int -> Int
freud' x = x
-- a->a can turn into Int->Int

myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX
-- Int->Int cannot turn into a->a

myX' = 1 :: Int
sigmund' :: Int -> Int
sigmund' x = myX'
-- Int->Int can turn into Num a => a->a

jung :: [Int] -> Int
jung xs = head (sort xs)
-- Ord a => [a] -> a can turn into [Int] -> Int

young :: Ord a => [a] -> a
young xs = head (sort xs)
-- [Char] -> Char can turn into Ord a => [a] -> a

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
-- [Char]->Char cannot turn into Ord a => [a] -> a


-- Type-Kwon-Do
chk :: Eq b => (a->b) -> a -> b -> Bool
chk f a b = f a == b
-- Example: chk (\x-> x+6) 5 11  == True

arith :: Num b => (a->b) -> Integer -> a -> b
arith f i a = f a + fromInteger i
-- Example arith (\x->x+5) 6 6  == 17
