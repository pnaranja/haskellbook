{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Lib
import Data.Int

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
-}

-- Unary Constructor
-- Cardinality of Goats is the cardinality of the type argument (Int)
data UnGoats = UnGoats Int deriving (Eq,Show)

-- newtype
-- Cardinality same as unary constructor
-- Not allowed to be product or sum type or contain nullary constructors
-- No runtime overhead compared to using data keyword - reuses representation
--  of the type it contains

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

-- But what if there's different limits for different animals
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats2 :: Goats -> Bool
tooManyGoats2 (Goats n) = n > 42

-- Difference between newtype and type alias
-- You can define typeclass instances of newtypes that differ from their
-- underlying type.  You can't do that for type aliases
class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

-- Only works if we assign Int to the literal
-- tooMany (42 :: Int)

-- Now you want different behavior for Goats - you can handle more!
-- Under the hood, it's still Int but the newtype declaration will allow
-- you a custom instance
instance TooMany Goats where
    tooMany (Goats n) = n > 70

-- tooMany (Goats 60) = False
-- tooMany (Goats 72) = True

-- Will not compile!!
-- type PaulInt = Int
-- instance TooMany PaulInt where
--     tooMany n = n > 60

-- Need Pragma {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- to derive class TooMany like this
newtype Goats2 = Goats2 Int deriving (Eq, Show, TooMany)

-- Exercise Logic Goats
newtype Pigs = Pigs (Int, String) deriving (Eq,Show)
instance TooMany Pigs where
    tooMany (Pigs (i, s)) = i>70 && s=="Pigs"

newtype TwoGoats = TwoGoats (Int,Int) deriving (Eq,Show)
instance TooMany TwoGoats where
    tooMany (TwoGoats (x,y)) = x+y> 140

-- From https://github.com/nackjicholson/haskellbook-solutions/blob/master/chapter11/exercises.hs
instance (Num a, TooMany a) => TooMany (a,a) where
    tooMany (x,y) = tooMany (x+y)


-- Sum Types --

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- Bool is cardinality is 2.  Since there is a Sum Type, BigSmall is
-- cardinality of 4

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
-- Cardinality of NumberOrBool is (Cardinality of Int8) + (Cardinality of
-- Bool) = 256 + 2 = 258

-- myNumba = Numba (-128) => -128 is a valid Int8 value but (-128) turns into
--  (negate 128) but evaluates 128 first and complains!
n = (-128)
myNumba = Numba n -- gives warning and suggests to use NegativeLiterals


-- Product Types --
-- Carry multiple values around in a single data constructor
-- Remember, Tuples are anonymous products! => (,) :: a -> b -> (a,b)
data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq,Show)
-- TwoQs has a data constructor (MkTwoQs) that takes two arguments =>
-- cardinality of 3*3=9
