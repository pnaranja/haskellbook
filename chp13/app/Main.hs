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

import Data.Monoid

old_main :: IO ()
old_main = do
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
data Puzzle = Puzzle String [Maybe Char] String

-- How to show the puzzle on the command line
-- First show the string with any correct guesses
-- Then show all the letters that have been guessed
instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        "\n" ++ intersperse ' ' (fmap renderPuzzleChar discovered)
            ++ "\nGuessed so far: " ++ intersperse ',' guessed ++ "\n"

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (fmap (const Nothing) str) [] 

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) char = elem char str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ str) char = elem char str

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a

-- For fillInCharacter function
instance Monoid Char where
    mempty = ' '
    mappend m1 _ = m1

-- Insert a CORRECTLY GUESSED character in the string
-- (fill in the '_' character(s)
-- Find out what matches in str, combine that list with the original discovered, and add char to guessed
-- Using instance of Monoid for characters defined above
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle str discovered guessed) char = Puzzle str newdiscovered newguessed
    where
        mapdiscovered = map (\c->if c==char then Just char else Nothing) str
        newdiscovered = zipWith mappend mapdiscovered discovered
        newguessed = guessed ++ [char]


-- Handle a guess from the user
-- This function will be used within the IO monad
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess p@(Puzzle str discovered guessed) char
  | alreadyGuessed p char = putStrLn ("You already guessed " ++ [char]) >>= (\_->return p)
  | charInWord p char = putStrLn "You guessed correctly!" >>= (\_->return (fillInCharacter p char))
  | otherwise = putStrLn "Incorrect!" >>= \_->return $ Puzzle str discovered (guessed ++ [char])

-- Stop game after certain number of guesses
gameOver :: Puzzle -> IO ()
gameOver (Puzzle str _ guessed)
  | length guessed > 7 = putStrLn ("You lost!\nYou've passed the limit of guesses\nThe word was: "++ str) >>=  const exitSuccess
  | otherwise = return ()

-- If all discovered has been turned from Nothing->Just x, then exit game
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _)
  | all isJust discovered = putStrLn "You win!" >>= const exitSuccess
  | otherwise = return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
    word <- getRandomWord
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
