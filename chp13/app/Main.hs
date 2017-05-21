module Main where

import DogsRule
import Hello (sayHello)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  sayHello name
  dogs

minWordLength = 5
maxWordLength = 9

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return $ filter acceptedLength aw
        where
            strLength x = length (x::String)
            acceptedLength x = strLength x > minWordLength && strLength x < maxWordLength

lengthGameWords :: IO Int
lengthGameWords = fmap length gameWords

-- Get random index for game words (index must be length - 1)
randomIndexInGameWords :: IO Int
randomIndexInGameWords = (\x->randomRIO (0,x)) =<< fmap (\x->x-1) lengthGameWords

randomWord :: WordList -> IO String
randomWord wl = (\x->return (wl !! x)) =<< randomIndexInGameWords

getRandomWord :: IO String
getRandomWord = randomWord =<< gameWords


-- Arguments:
-- 1. String the user is trying to guess
-- 2. Characters user has picked correctly
-- 3. Characters user has chosen so far
data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        "\n" ++ (intersperse ' ' $ fmap renderPuzzleChar discovered) 
            ++ "\nGuessed so far: " ++ (intersperse ',' guessed) ++ "\n"

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (fmap (const Nothing) str) [] 

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) char = elem char str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ str) char = elem char str

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a
