module Main where

import DogsRule
import Hello (sayHello)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  sayHello name
  dogs