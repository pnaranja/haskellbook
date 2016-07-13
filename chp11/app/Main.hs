module Main where

import Lib

main :: IO ()
main = someFunc

{-
    Given:
    data DogueDeBordeaux doge = DogueDeBordeaux doge

    data Doggies a =
    Husky a
    | Mastiff a
    deriving (Eq, Show)

    1. Is Doggies a type constructor or a data constructor?
        Doggies is a type constructor
    2. What is the kind of Doggies?
        * -> *
    3. What is the kind of Doggies String?
        * -> *
    4. What is the type of Husky 10?
        Num a => Doggies a
    5. What is the type of Husky (10 :: Integer)?
        Integer a => Doggies a
    6. What is the type of Mastiff "Scooby Doo"?
        Doggies String
    7. Is DogueDeBordeaux a type constructor or a data constructor?
        Both
    8. What is the type of DogueDeBordeaux?
        doge -> DogueDeBordeaux doge
    9. What is the type of DogueDeBordeaux "doggie!"
        DogueDeBordeaux String
-}

--Exercises Vehicles
data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata | PlaneManu deriving (Eq, Show)
data Airline = PapuAir | CatapultsRUs | TakeYourChancesUnited deriving (Eq, Show)
type Size = Integer

data Vehicle =
    Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
myPlane = Plane PapuAir 1000
unitedPlane = Plane TakeYourChancesUnited 900

isCar :: Vehicle -> Bool
isCar (Car _ _)  =  True
isCar  _         =  False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _)  =  True
isPlane  _         =  False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer
getManu _ = PlaneManu -- "Default" manufacturer

data ProductExample = Example Int String deriving (Eq,Show)

-- Algebraic datatypes - Describe the argument patterns in sum and product
-- Cardinality of datatype - possible values it defines
--  Example: Bool -> 2 , Int8 -> range is -128-127 so it's 256
{--
    Cardinality
    1. data PugType = PugData -> 1
    2. data Airline = PapuAir | CatapultsRUs | TakeYourChancesUnited -> 3
    3. Int16 -> 65536
    4. Int -> 18446744073709551614
- }