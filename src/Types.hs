module Types where

import Data.Time (Day)

data FileInfo = FileInfo {
    totalListened :: Integer,
    albumsPerDate :: [(Day, Integer)],
    artists :: [Artist]
} deriving (Show)

data Artist = Artist {
    artistName :: String,
    numListened :: Integer,
    albums :: [Album]
} deriving (Show)

data Album = Album {
    albumName :: String,
    dateListenedTo :: Maybe Day
} deriving (Show)
