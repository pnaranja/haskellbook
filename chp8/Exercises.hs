-- Exercises and Notes for Chapter 8
main :: IO ()
main = undefined

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times-1) n)

--Extract recursion of incTimes
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
--applyTimes n f b = f (applyTimes (n-1) f b) -- Old implementation
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

--Write out evaluation of applyTimes 5 (+1) 5
-- ((+1) ((+1) ((+1) ((+1) ((+1) 5)))))
-- (+1) . (+1) . (+1) . (+1) . (+1) $ 5


-- bottom: Computations that do not result in a value
-- because it failed with an error or does not terminate
-- i.e. let x = x in x

-- i.e. Anything that throws an exception means no real value returned
f_bottom :: Bool -> Int
f_bottom True = error "Going to throw Exception"
f_bottom False = 0

-- i.e: Partial function - does not take into account f_bottom True
f_bottom2 :: Bool -> Int
f_bottom2 False = 0

-- Change partial function to handle all possible inputs
-- One way, use Maybe datatype
data MyMaybe a = Just a | Nothing
f :: Bool -> MyMaybe Int
f False = Main.Just 0
f _ = Main.Nothing

-- Fibonacci
-- input and output are positive whole numbers
-- Determine "base case(s)" - how it will terminate
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

-- Division through recursive subtraction
-- Create readable types
-- Adding up the number of times you substract (Numerator-Denominator)
-- Stop subtracting if difference is equal or less than 0
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

divideBy :: Numerator -> Denominator -> Quotient
divideBy n d
 | n-d < 0 = 0
 | n-d == 0 = 1
 | otherwise = 1 + divideBy (n-d) d

-- Use common "go" idiom to create sort of "recusive tail call"
divideBy' :: (Integral a) => a -> a -> (a,a)
divideBy' n d           =  go n d 0
    where go n d count
            | n < d         =  (count, n)
            | otherwise     =  go (n-d) d (count+1)

-- Recusively sum xumbers from 1 to x
recusiveSum :: (Eq a, Num a) => a -> a
recusiveSum x = go x 1 0
    where go n count thesum
            | n == 0 = thesum
            | otherwise = go (n-1) (count+1) (thesum+count)


recusiveSum' :: Int -> Int
recusiveSum' x = sum . take x . enumFrom $ 1

-- Recusively multiply summing recusively
multiplyBySum :: (Integral a) => a -> a -> a
multiplyBySum x y = go x y x
    where go a b thesum
           | a==0 || b==0  =  0
           | a == 1        =  b
           | b == 1        =  thesum
           | otherwise     =  go a (b-1) (thesum+a)


-- Fix deviceBy' function
-- Make sure it handles divide by 0
-- and negative numbers

data DivideResult = Result Integer | DividedByZero
    deriving Show

negDivideResult :: (Integral a) => (DivideResult,a) -> (DivideResult,a)
negDivideResult (Result c, d)  =  (Result ((-1) * c), d)
negDivideResult (DividedByZero, d)  =  (DividedByZero, d)

divideBy'' :: (Integral a) => a -> a -> (DivideResult,a)
divideBy'' n d           =  go n d 0
    where go :: (Integral a) => a -> a -> a -> (DivideResult,a) 
          go n d count
            | d == 0     =  (DividedByZero, count)
            | d < 0     =  negDivideResult . divideBy'' n $ (d * (-1))
            | n < 0     =  negDivideResult . divideBy'' (n * (-1)) $ d
            | n < d      =  (Result (toInteger count), n)
            | otherwise  =  go (n-d) d (count+1)
