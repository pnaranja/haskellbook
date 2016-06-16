module DBProcessing where

import Data.Time

data DatabaseItem = DBString String
    | DBNumber Integer
    | DBDate UTCTime
    deriving (Show, Eq, Ord)

theDatabase :: [DatabaseItem]
theDatabase = 
    [DBDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
        DBNumber 9001,
        DBString "Hello World!",
        DBDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

isDBDate :: DatabaseItem -> Bool
isDBDate (DBDate _) = True
isDBDate _ = False

filterDBDate :: [DatabaseItem] -> [UTCTime]
filterDBDate = map (\(DBDate theTime) -> theTime) . filter isDBDate

isDBNumber :: DatabaseItem -> Bool
isDBNumber (DBNumber _) = True
isDBNumber _ = False

filterDBNumber :: [DatabaseItem] -> [Integer]
filterDBNumber = map (\(DBNumber theNum) -> theNum) . filter isDBNumber

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (\(DBDate t) -> t) . foldr getRecent (DBDate startUTCTime) . filter isDBDate

startUTCTime :: UTCTime
startUTCTime = UTCTime (fromGregorian 0000 0 0) (secondsToDiffTime 0) 

getRecent :: DatabaseItem -> DatabaseItem -> DatabaseItem
getRecent (DBDate t1) (DBDate t2)
  | t1<t2 = DBDate t2
  | otherwise = DBDate t1
getRecent (DBDate t1) _ = DBDate t1
getRecent _ (DBDate t1) = DBDate t1
getRecent _ _ = DBDate startUTCTime
