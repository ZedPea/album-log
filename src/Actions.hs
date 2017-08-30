module Actions
(
    add,
    addInteractive
)
where

import Control.Lens ((^..), (%~), (&), (^.), (.~), (+~), traversed)
import Data.Time (Day, getCurrentTime, utctDay)
import System.IO (hFlush, stdout)

import Types
import Sort (sortFileInfo)
import Utilities (addEscape)

addInteractive :: FileInfo -> IO FileInfo
addInteractive f = do
    putStr "Artist name: "
    hFlush stdout
    artist <- getLine

    putStr "Album name: "
    hFlush stdout
    album <- getLine

    add f artist album

add :: FileInfo -> String -> String -> IO FileInfo
add f artist album = do
    date <- utctDay <$> getCurrentTime
    return $ if artist' `elem` artistNames
        then appendAlbum f artist' album' date
        else insertArtist f artist' album' date
    where artistNames = f^..artists.traversed.artistName
          artist' = addEscape artist
          album' = addEscape album

appendAlbum :: FileInfo -> String -> String -> Day -> FileInfo
appendAlbum f artist album date = sortFileInfo $ 
    f & artists.traversed %~ appendAlbum'
    where appendAlbum' a
            | a^.artistName == artist = a & albums .~ (newAlbum : a^.albums)
                                          & numListened +~ 1
            | otherwise = a
          newAlbum = Album album (Just date)

insertArtist :: FileInfo -> String -> String -> Day -> FileInfo
insertArtist f artist album date = sortFileInfo $
    f & artists .~ (newArtist : f^.artists)
    where newArtist = Artist artist 1 [Album album (Just date)]
