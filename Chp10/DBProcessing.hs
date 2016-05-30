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

filterDBDate :: [DatabaseItem] -> [UTCTime]
filterDBDate = undefined
