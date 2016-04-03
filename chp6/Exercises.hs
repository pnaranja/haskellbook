-- Exercises for Chapter 6

main :: IO ()
main = undefined

-- Intermission Exercises
data TisAnInteger = TisAnInteger Integer
instance Eq TisAnInteger where
    (==) (TisAnInteger a) (TisAnInteger a') = a == a'

data TwoIntegers = TwoIntegers Integer Integer
instance Eq TwoIntegers where
    (==) (TwoIntegers a b) (TwoIntegers a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = a == a'
    (==) (TisAString a) (TisAString a') = a == a'
    (==) _ _ = False

data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) (ThisOne a) (ThatOne a') = a == a'
    (==) (ThatOne a) (ThisOne a') = a == a'

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) (Hello _) (Goodbye _) = False
    (==) (Goodbye _) (Hello _) = False

--Chapter Exercises
-- TypeCheck
data Person = Person Bool
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
-- Should not type check because person is not an instance of Show
instance Show Person where
    show (Person a) = show a

data Mood = Blah | Woot deriving Show
settleDown x = if x == Woot
                  then Blah
                  else x
-- Should not type check because x can be infinite types, also no Eq type class instance of Mood
instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _ = False
