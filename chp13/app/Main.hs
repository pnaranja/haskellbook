module Main where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

import Control.Monad (forever)
import Data.Char (toLower, ord)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Data.Monoid

minWordLength = 5
maxWordLength = 9

newtype WordList = WordList [String] deriving (Eq,Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords -- Destructuring WordList
    return $ WordList (filter acceptedLength aw)
        where
            strLength x = length (x::String)
            acceptedLength x = strLength x > minWordLength && strLength x < maxWordLength

lengthGameWords :: IO Int
lengthGameWords = do
    (WordList words) <- gameWords
    return (length words)

-- Get random index for game words (index must be length - 1)
randomIndexInGameWords :: IO Int
randomIndexInGameWords = (\x->randomRIO (0,x)) =<< fmap (\x->x-1) lengthGameWords

randomWord :: WordList -> IO String
randomWord (WordList wl) = 
    (\x->return (wl !! x)) =<< randomIndexInGameWords

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
            ++ "\nWrong guesses so far: " ++ intersperse ',' guessed ++ "\n"

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (fmap (const Nothing) str) [] 

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) char = elem char str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ discovered str) char = elem char (str ++ discoveredToStr discovered)

discoveredToStr = map (\x->fromMaybe ' ' x)

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
fillInCharacter (Puzzle str discovered guessed) char = Puzzle str newdiscovered guessed
    where
        mapdiscovered = map (\c->if c==char then Just char else Nothing) str
        newdiscovered = zipWith mappend mapdiscovered discovered


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
    putStrLn $ "\nCurrent puzzle is: " ++ show puzzle
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

--EXTRA STUFF1--
palindrome :: IO () 
palindrome = forever $ do
    putStrLn "\nEnter a phrase and I'll check if it's a palindrome"
    line1 <- getLine
    line2 <- return $ lowerAndConstraintToLetters line1
    case line2 == reverse line2 of
        True -> putStrLn "It's a palindrome!" 
        False -> putStrLn "Not a palindrome!" >>=  const exitSuccess

constrainToLetters :: Char -> Bool
constrainToLetters c
 | x >= 97 && x <= 122 = True
 | otherwise = False
 where
     x = ord c

lowerAndConstraintToLetters = (filter constrainToLetters . map toLower)


--EXTRA STUFF2--
type Name = String 
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                    | AgeTooLow
                    | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age 
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

-- Prompt user for name and age
-- Try to create Person.  If successful, print ”Yay! Successfully got a person:” followed by the Person value.
-- If fail, print error
gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter Name: "
    name <- getLine
    putStr "Enter Age: "
    age <- fmap read getLine :: IO Integer
    putStrLn $ determinePerson $ mkPerson name age

determinePerson :: Either PersonInvalid Person -> String
determinePerson (Right (Person name age)) = "Yay! Successfully got a person: " ++ name ++ " - " ++ show age
determinePerson (Left NameEmpty) = "Sorry, Name was empty"
determinePerson (Left AgeTooLow) = "Sorry, Age too low"
determinePerson (Left (PersonInvalidUnknown error)) = error
