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

getDBDate :: DatabaseItem -> UTCTime
getDBDate (DBDate theTime) = theTime

filterDBDate :: [DatabaseItem] -> [UTCTime]
filterDBDate = map getDBDate . (filter isDBDate)
