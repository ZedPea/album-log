{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Time (Day)
import Data.Char (toLower)
import Control.Lens (makeLenses, (^.))

data FileInfo = FileInfo {
    _totalListened :: Integer,
    _albumsPerDate :: [(Day, Integer)],
    _artists :: [Artist]
} deriving (Show)

data Artist = Artist {
    _artistName :: String,
    _numListened :: Integer,
    _albums :: [Album]
} deriving (Show, Eq)

data Album = Album {
    _albumName :: String,
    _dateListenedTo :: Maybe Day
} deriving (Show, Eq)

--have to derive lenses before using in instance
makeLenses ''FileInfo
makeLenses ''Artist
makeLenses ''Album

instance Ord Album where
    compare a b = compare (low $ a^.albumName) (low $ b^.albumName)

instance Ord Artist where
    compare a b = compare (low $ a^.artistName) (low $ b^.artistName)

--case insensitive sorting
low :: String -> String
low = map toLower
