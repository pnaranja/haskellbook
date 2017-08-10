module Main where

import Data.Monoid

main :: IO ()
main = do
  putStrLn "hello world"

-- class Monoid m where
-- mempty :: m
-- mappend :: m -> m -> m
-- mconcat :: [m] -> m
-- mconcat = foldr mappend mempty


t1 = mappend [1,2,3] [4,5,6]
t2 = mconcat [[1..3],[4..6]]


t3 = foldr (++) [] [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]
t4 = foldr mappend mempty [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]
--
-- Instance Monoid [] where
-- mempty = []
-- mappend = (++)
--

-- mappend 1 1
-- Will not work because not sure if the operation is addition or multiplication
-- Need Sum and Product newtypes
t5 = mappend (Sum 1) (Sum 6)
t6 = mappend (Sum 2.5) (Sum 6.6)

-- So integers form a Monoid under addition and multiplication
-- Like Lists form a Monoid under concatenation
--
--
-- mappend is (<>)
-- Data.Monoid> :t mappend
-- mappend :: Monoid a => a -> a -> a
-- Data.Monoid> :t (<>)
-- (<>) :: Monoid m => m -> m -> m
--
-- Data.Monoid> "Hello" <> " Paul"
-- "Hello Paul"
-- Data.Monoid> mappend "Hello" " Paul"
-- "Hello Paul"

t7 = mappend (Sum 8) (mappend (Sum 2) (Sum 5))
t8 = Sum 8 <> Sum 2 <> Sum 5
t9 = mconcat [Sum 8,Sum 2,Sum 5]
t10 = getSum $ mconcat [Sum 8,Sum 2,Sum 5]

-- Monoid Laws
-- Identity
t11 = (Sum 1) <> mempty
t12 = mempty <> (Sum 1) 

-- Associativity
t13 = Sum 1 <> (Sum 2 <> Sum 3)
t14 = (Sum 1 <> Sum 2) <> Sum 3

-- Boolean Monoids
t15 = All True <> All True <> All False
t16 = Any True <> Any False <> Any False

-- First and Last Monoids
t17 = First (Just 1) <> First (Just 2)
t18 = Last (Just 1) <> Last (Just 2)
t19 = First (Nothing) <> First (Just 2)
t20 = Last (Just 1) <> Last (Nothing)
t21 = Last (Nothing) <> Last (Nothing)


-- Reusing algebras by asking for algebras
-- instance (Monoid a, Monoid b) => Monoid (a,b)
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c)
--
-- It's ok if a,b,c cannot 'mappend'
-- Below, False' and True' are not a Monoid, but that's ok
data Booly a = False' | True' deriving (Eq, Show)
instance Monoid (Booly a) where
    mempty = True'
    mappend False' _ = False'
    mappend _ False' = False'
    mappend True' True' = True'


-- Exercise: Optional Monoid
data Optional a = Nada | Only a deriving (Eq,Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada Nada = Nada
    mappend Nada (Only a) = Only a
    mappend (Only a) Nada = Only a
    mappend (Only a) (Only b) = Only (a <> b)

t22 = Only (Sum 1) <> Only (Sum 1)
t23 = Only (Product 4) <> Only (Product 2)
t24 = Only (Sum 1) <> Nada
t25 = Only [1] <> Nada
t26 = Nada <> Only (Sum 1)


-- Madness (Mad Libs)
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj = 
    e <> "! he said \'" <>
    adv <> "\' as he jumped into his " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = 
    mconcat[e,"! he said \'",adv,"\' as he jumped into his ",noun," and drove off with his ",adj," wife."]

