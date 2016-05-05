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


