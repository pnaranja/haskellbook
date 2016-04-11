-- Exercises and Notes for Chapter 7
main :: IO ()
main = undefined

-- Lexical scope
x    =  5
y    =  5 + x
z y  =  y * 10

-- z 9  == 90 ; y in the function z is 9
-- z y == 100 ; Since no VALUE is passed to z, y from the outer most scope (y=5+x) is used

-- Anonymous Functions
addOneIfOdd n = case odd n of
                    True -> f n
                        where f = \x'->x'+1
                    False -> n

addFive :: Int -> Int -> Int
addFive = \x y'-> if x>y' then y'+5 else x+5

-- Pattern Matching
f :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f  (a,b,c) (d,e,f) = ((a,d), (c,f))

-- Case expressions
functionC x y = case (x>y) of
                  True -> x
                  False -> y

ifEvenAdd2 x = case (even x) of
                 True -> x+2
                 False -> x

nums x y = case (compare x y) of
           LT -> -1
           GT -> 1
           EQ -> 0

-- Guards
pal xs 
  | xs == (reverse xs) = True
  | otherwise = False

-- Chapter Exercises
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast =  (div) x 10
          d = (mod) xLast 10

tensDigit' = \x -> snd . (\y-> divMod y 10) . fst . divMod x $ 10
tensDigit'' = \x -> (snd . (\y-> divMod y 10) . fst . divMod x) 10

hunsD = \x -> snd . divMod x $ 10
