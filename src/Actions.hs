module Actions
(
    add,
    createFile,
    remove
)
where

import Control.Lens ((^..), (%~), (&), (^.), (.~), (+~), (-~), traversed)
import Data.Time (Day, getCurrentTime, utctDay)
import Data.Maybe (isJust, fromJust)
import Data.Char (toLower)

import Types
import Sort (sortFileInfo)
import Utilities (addEscape, alreadyExists)

createFile :: FileInfo
createFile = FileInfo [] 0 [] []

add :: FileInfo -> String -> String -> IO FileInfo
add f artist' album' = do
    date <- utctDay <$> getCurrentTime

    if isDupe f artist album
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
    where f | d == fst (head mapping) = tail
            | otherwise = id

remove :: FileInfo -> String -> String -> Either String FileInfo 
remove f artist album
    | isJust foundArtist && 
      album `elem` map (map toLower) albumNames = Right $ remove' f artist album
    | otherwise = Left "Artist/Album combo not found!"
    where foundArtist = getArtist (f^.artists)
          getArtist [] = Nothing
          getArtist (x:xs)
            | map toLower (x^.artistName) == artist = Just x
            | otherwise = getArtist xs
          albumNames = fromJust foundArtist^..albums.traversed.albumName

remove' :: FileInfo -> String -> String -> FileInfo
remove' f artist album = f & artists %~ map (removeAlbum artist album)
                           & totalListened -~ 1
                           & albumsPerDate %~ updateDateListened

removeAlbum :: String -> String -> Artist -> Artist
removeAlbum artist album a
    --don't actually need to check that it exists, as we have already checked
    | map toLower (a^.artistName) == artist = 
      a & albums .~ filtered (a^.albums)
        & numListened -~ 1
    | otherwise = a
    where filtered = filter (\x -> map toLower (x^.albumName) /= album)

--this does invalidate the correctness of the date / album mapping, but it's
--quite annoying to extract the date an album was listened to whilst updating 
-- - if it even has a date
updateDateListened :: [(Day, Integer)] -> [(Day, Integer)]
updateDateListened x
    | null x = []
    | length x == 1 = [(d, i-1)]
    | i - 1 == i' = tail x -- one album on this day, so remove
    | otherwise = (d, i-1) : tail x -- more than one, so just take one off
    where (d, i) = head x
          (_, i') = x !! 1
