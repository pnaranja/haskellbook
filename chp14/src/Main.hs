module Main where

import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
    describe "Divided by test1" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5,0)
        it "22 divided by 5 is 4 reminder 2" $ do
            dividedBy 22 5 `shouldBe` (4,2)


dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
    where go n d count
            | n<d = (count, n)
            | otherwise = go (n-d) d (count+1)

-- Multiply two numbers using recusive summation
summation :: (Eq a, Num a) => a -> a -> a
summation a b =  mul a b a
    where mul x y xorig
            | x == 0 || y == 0 = 0
            | x == 1 = y
            | y == 1 = x
            | otherwise = mul (x+xorig) (y-1) xorig

testSummation = hspec $ do
    describe "Test Summation" $ do
        it "5 times 20 is 100" $ 
            summation 5 20 `shouldBe` 100
        it "3 times 0 is 0" $ 
            summation 3 0 `shouldBe` 0

testSummation2 = hspec $ do
    describe "Test Summation" $ 
        (it "5 times 20 is 100" $ summation 5 20 `shouldBe` 100) 
        >>= (\x->it "3 times 0 is 0" $ summation 3 0 `shouldBe` 0)
        >>= (\x->it "1 times 10000 is 10000" $ summation 1 10000 `shouldBe` 10000)


testQuick1 = hspec $ do
    describe "Testing QuickCheck" $ do
        it "x + 1 is greater than x" $
            property $ \x-> x + 1 > (x::Int)
        it "x - 1 is less than x" $
            property $ \x-> x - 1 < (x::Int)

-- QuickCheck relies on typeclass Arbitrary and newtype Gen generating random data
-- Use sample to see the random data
-- sample :: Show a => Gen a -> IO ()
-- sample (arbitary :: Gen Int)

-- A trivial random generator of 1's , 1,2,3's and a-z
-- run: sample' trivialGenInt
trivialGenInt :: Gen Int
trivialGenInt = return 1

-- elements :: [a] -> Gen a
oneTwoThree :: Gen Int
oneTwoThree = elements [1,2,3,3,3,2,2,2,2,1]

oneTwoThreePlusOne :: Gen Int
oneTwoThreePlusOne = fmap (+1) oneTwoThree

genLetters :: Gen Char
genLetters = elements ['a'..'z']

-- choose :: System.Random.Random a => (a,a) -> Gen a
-- Use choose to randomly choose from a tuple
genBool :: Gen Bool
genBool = choose (True,False)

genBool' :: Gen Bool
genBool' = elements [False,True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]


-- Generators with polymorphic type arguments

-- sample' (genTuple :: Gen (Int,Float))
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a,b)

-- sample' (genMaybe :: Gen (Maybe Int))
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- Using QuickCheck without Hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = 5 `div` x  > x - 1

runQc :: IO ()
runQc = quickCheck prop_additionGreater

--
--
-- 14.6 Kicking around QuickCheck
data Trivial = Trivial deriving (Eq,Show)

-- Need "return" to put Trivial in the Gen monad
trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

-- sample trivialGen results in 10 Trivials
-- Gen values are generators of random values that QuickCheck uses to get test values from


data Identity a = Identity a deriving (Eq,Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return $ Identity a

identityGen' :: Arbitrary a => Gen (Identity a)
identityGen' = arbitrary >>= (\a->return $ Identity a)

-- Eta reduce?
identityGen'' :: Arbitrary a => Gen (Identity a)
identityGen'' = arbitrary >>= return . Identity

-- "mapping" (Identity a) to Gen a -> Gen (Identity a)
identityGen''' :: Arbitrary a => Gen (Identity a)
identityGen''' = fmap Identity arbitrary


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

-- sample identityGenInt results in 10 Identity <random nums>
identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenStr :: Gen (Identity String)
identityGenStr = identityGen

--
-- Arbitrary Products
--

data Pair a b = Pair a b deriving (Show,Eq)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

pairGen' :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen' = arbitrary >>= (\a->arbitrary >>= (\b->return $ Pair a b)) 

pairGen'' :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen'' = arbitrary >>= (\a->arbitrary >>= return . Pair a)

pairGenInString :: Gen (Pair Int String)
pairGenInString = pairGen
