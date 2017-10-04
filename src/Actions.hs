module Actions
(
    add,
    createFile,
    remove
)
where

import Data.Time (Day, getCurrentTime, getTimeZone, utcToLocalTime, localDay)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Data.Monoid (Any(..))
import Control.Lens 
    ((^..), (^.), (+=), (%=), (-=), (.=), (&), (+~), (%~), traversed, filtered, 
     zoom)

import Types (AlbumStateT, FileInfo(..), Artist(..), Album(..))
import Sort (sortFileInfo)
import Utilities (addEscape, alreadyExists)

import Lenses 
    (totalListened, albumsPerDate, artists, albums, albumName, artistName,
     numListened)

createFile :: FileInfo
createFile = FileInfo [] 0 [] []

add :: String -> String -> AlbumStateT ()
add artist' album' = do
    utcTime <- lift getCurrentTime
    zone <- lift $ getTimeZone utcTime

    let day = localDay $ utcToLocalTime zone utcTime

    dupe <- isDupe artist album

    if dupe
        then lift . putStrLn $ alreadyExists artist album
        else do
            appendAlbum artist album day
            updateAlbumDate day
            totalListened += 1
            sortFileInfo

    where artist = addEscape artist'
          album = addEscape album'

isDupe :: String -> String -> AlbumStateT Bool
isDupe artist album = do
    s <- get
    return $ album `elem` (s^..artists.traversed.filtered isArtist.albumNames)
    where isArtist a = a^.artistName == artist
          albumNames = albums.traversed.albumName

appendAlbum :: String -> String -> Day -> AlbumStateT ()
appendAlbum artist album day = do
    s <- get
    if artist `elem` (s^..artists.traversed.artistName)
        then artists.traversed %= someFunc
        else artists %= (\a -> Artist artist 1 [Album album (Just day)] : a)

    where isArtist a = a^.artistName == artist
          someFunc a
            | isArtist a = a & albums %~ (\x -> Album album (Just day) : x)
                             & numListened +~ 1
            | otherwise = a

updateAlbumDate :: Day -> AlbumStateT ()
updateAlbumDate d = do
    s <- get

    let n = s^.totalListened+1

    albumsPerDate %= (\x -> if null x then [(d, n)]
                                else if sameDate s
                                    then (d, n) : tail x
                                    else (d, n) : x)

    where sameDate s = d == (fst . head $ s^.albumsPerDate)

remove :: String -> String -> AlbumStateT (Maybe String)
remove artist album = do
    result <- zoom (artists.traversed.filtered isArtist) $ Any <$> do
        s <- get
        if album `elem` (s^..albums.traversed.albumName)
            then do
                albums %= someFunc
                numListened -= 1
                return True
            else return False

    s <- get

    artists .= s^..artists.traversed.filtered (\a -> a^.numListened > 0)

    if getAny result
        then do
            totalListened -= 1
            updateDateListened
            return Nothing
        else return $ Just "Artist/Album combo not found!"

    where isArtist a = a^.artistName == artist
          someFunc [] = []
          someFunc (a:as)
            | a^.albumName == album = as
            | otherwise = a : someFunc as

--this does invalidate the correctness of the date / album mapping, but it's
--quite annoying to extract the date an album was listened to whilst updating 
-- - if it even has a date
updateDateListened :: AlbumStateT ()
updateDateListened = albumsPerDate %= modify
    where modify x
            | null x = []
            | length x == 1 = [(d, i-1)]
            | i - 1 == i' = tail x -- one album on this day, so remove
            | otherwise = (d, i-1) : tail x -- more than one, so just take one off
            where (d, i) = head x
                  (_, i') = x !! 1
