module Main where

import qualified Exercises (treeBuild)
import qualified Exercises as T

main :: IO ()
main = do
  print $ show $ Exercises.treeBuild 1
  print $ show $ T.treeBuild 1

