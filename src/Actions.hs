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
import Utilities (addEscape, alreadyExists)

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
add f artist' album' = do
    date <- utctDay <$> getCurrentTime

    let dupe = isDupe f artist album

    if dupe
        then putStrLn (alreadyExists artist album) >> return f
        else do
            let new | artist `elem` artistNames = apply appendAlbum
                    | otherwise = apply insertArtist
                    where apply func = func f artist album date

            return $ new & totalListened +~ 1
                         & albumsPerDate %~ updateAlbumDates date 
                                            (f^.totalListened)

    where artistNames = f^..artists.traversed.artistName
          artist = addEscape artist'
          album = addEscape album'

isDupe :: FileInfo -> String -> String -> Bool
isDupe f artist album = isDupe' (f^.artists)
    where isDupe' [] = False
          isDupe' (x:xs)
            | x^.artistName == artist && album `elem` x^..albumNames = True
            | otherwise = isDupe' xs
          albumNames = albums.traversed.albumName

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

updateAlbumDates :: Day -> Integer -> [(Day, Integer)] -> [(Day, Integer)]
updateAlbumDates d n [] = [(d, n+1)] -- n can only really be 0 in this case
                                     -- because if there are no date markings
                                     -- then no albums have been listened to.
                                     -- It could also be a dodgy file i guess
                                     -- so might as well increment n instead

updateAlbumDates d n mapping = (d, n+1) : f mapping 
    where f
            | d == fst (head mapping) = tail
            | otherwise = id
