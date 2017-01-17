module Main where

main :: IO ()
main = putStrLn "Start chp12"

-- Smart Constructors
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
-- But we can make an empty string for Name and a negative number for Age!
-- Lets constrain this with Maybe
-- mkPerson is considered a "Smart Constructor"
mkPerson :: Name -> Age -> Maybe Person
mkPerson n a
    | n /= "" && a >= 0 = Just $ Person n a
    | otherwise = Nothing

-- Use the Either datatype to tell the user what exactly is wrong when trying to construct Person
-- Left is our "Error Constructor"
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq,Show)

mkPerson2 :: Name -> Age -> Either PersonInvalid Person
mkPerson2 n a
    | n == "" = Left NameEmpty
    | a <= 0 = Left AgeTooLow
    | otherwise = Right $ Person n a

-- Notice that if Name AND AGE are invalid, it will only show NameEmpty!
-- Lets fix this and show the user both errors!  Make seperate checking and combine results
type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
    | age >= 0 = Right age
    | otherwise = Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay n
  | n /= "" = Right n
  | otherwise = Left [NameEmpty]

mkPerson3 :: Name -> Age -> ValidatePerson Person
mkPerson3 n a = mkPerson3' (nameOkay n) (ageOkay a)

mkPerson3' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson3' (Right n) (Right a) = Right (Person n a)
mkPerson3' (Left [NameEmpty]) (Right a) = Left [NameEmpty]
mkPerson3' (Right n) (Left [AgeTooLow]) = Left [AgeTooLow]
mkPerson3' (Left [NameEmpty]) (Left [AgeTooLow]) = Left [NameEmpty,AgeTooLow]
