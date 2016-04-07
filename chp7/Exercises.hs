-- Exercises and Notes for Chapter 7
main :: IO ()
main = undefined

-- Lexical scope
x    =  5
y    =  5 + x
z y  =  y * 10

-- z 9  == 90 ; y in the function z is 9
-- z y == 100 ; Since no VALUE is passed to z, y from the outer most scope (y=5+x) is used
