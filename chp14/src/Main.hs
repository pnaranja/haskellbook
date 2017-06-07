module Main where

import Test.Hspec

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
        (it "5 times 20 is 100" (summation 5 20 `shouldBe` 100)) 
        >>= (\x->it "3 times 0 is 0" $ summation 3 0 `shouldBe` 0)
        >>= (\x->it "1 times 10000 is 10000" $ summation 1 10000 `shouldBe` 10000)