module Types where

import Data.Time (Day)
import Data.Char (toLower)

data FileInfo = FileInfo {
    totalListened :: Integer,
    albumsPerDate :: [(Day, Integer)],
    artists :: [Artist]
} deriving (Show)

data Artist = Artist {
    artistName :: String,
    numListened :: Integer,
    albums :: [Album]
} deriving (Show, Eq)

data Album = Album {
    albumName :: String,
    dateListenedTo :: Maybe Day
} deriving (Show, Eq)

instance Ord Album where
    compare a b = compare (low $ albumName a) (low $ albumName b)

instance Ord Artist where
    compare a b = compare (low $ artistName a) (low $ artistName b)

--case insensitive sorting
low :: String -> String
low = map toLower
