module Decode
(
    decode
)
where

import Data.List (intercalate, isPrefixOf)
import Data.Time (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Printf (printf)
--import Data.List.Utils (replace)

import Types

decode :: FileInfo -> String
decode f = printf decodeFormatString total num comment albumDateMap 
                  artistAlbumMap

    where num = totalListened f
          comment = "# EPs included, singles not included."
          albumDateMap = intercalate "\n" $ decodeDates f
          artistAlbumMap = intercalate "\n" $ decodeArtists f
          total = "# TOTAL ALBUMS LISTENED TO"

decodeDates :: FileInfo -> [String]
decodeDates f = map decodeDate $ albumsPerDate f
    where decodeDate (d, num) = printf "# %s - %d" (formatDate d) num

decodeArtists :: FileInfo -> [String]
decodeArtists f = map decodeArtist $ artists f

decodeArtist :: Artist -> String
decodeArtist a = printf decodeArtistFormatString name num albums'
    where name = addDash $ artistName a
          num = numListened a
          albums' = intercalate "\n" (map decodeAlbum $ albums a)

decodeAlbum :: Album -> String
decodeAlbum a = printf decodeAlbumFormatString name date
    where name = addDash $ albumName a
          date = maybeDate $ dateListenedTo a

          maybeDate (Just d) = " - " ++ formatDate d
          maybeDate Nothing = ""

formatDate :: Day -> String
formatDate = formatTime defaultTimeLocale "%d/%m/%Y"

decodeFormatString :: String
decodeFormatString = "%s - %d\n%s\n\n%s\n%s\n"

decodeArtistFormatString :: String
decodeArtistFormatString = "\n%s - %d\n%s"

decodeAlbumFormatString :: String
decodeAlbumFormatString = "\t%s%s"

addDash :: String -> String
addDash = replace "\\" "\\-"

--the below functions are taken from MissingH/Data.List.Utils
--didn't want to have a large dependency for one function
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str
    | null remainder = [firstline]
    | remainder == delim = [firstline, []]
    | otherwise = firstline : split delim (drop (length delim) remainder)
    where (firstline, remainder) = breakList (isPrefixOf delim) str

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . split old

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs)
    | func list = (x:ys, zs)
    | otherwise = ([], list)
    where (ys,zs) = spanList func xs
