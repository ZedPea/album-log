module Types where

import Data.Time (Day)
import Data.Char (toLower)
import Control.Monad.Trans.State (StateT, State)

type AlbumStateT a = StateT FileInfo IO a
type AlbumState a = State FileInfo a

data FileInfo = FileInfo {
    _comments :: [String],
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

instance Ord Album where
    compare a b = compare (low $ _albumName a) (low $ _albumName b)

instance Ord Artist where
    compare a b = compare (low $ _artistName a) (low $ _artistName b)

--case insensitive sorting
low :: String -> String
low = map toLower
