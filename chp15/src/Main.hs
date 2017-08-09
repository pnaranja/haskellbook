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
