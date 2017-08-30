module Decode
(
    decode
)
where

import Data.List (intercalate)
import Data.Time (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Printf (printf)
import Control.Lens ((^.))

import Types
import Utilities (addDash)

decode :: FileInfo -> String
decode f = printf decodeFormatString total num comment albumDateMap 
                  artistAlbumMap

    where num = f^.totalListened
          comment = "# EPs included, singles not included."
          albumDateMap = intercalate "\n" $ decodeDates f
          artistAlbumMap = intercalate "\n" $ decodeArtists f
          total = "# TOTAL ALBUMS LISTENED TO"

decodeDates :: FileInfo -> [String]
decodeDates f = map decodeDate $ f^.albumsPerDate
    where decodeDate (d, num) = printf "# %s - %d" (formatDate d) num

decodeArtists :: FileInfo -> [String]
decodeArtists f = map decodeArtist $ f^.artists

decodeArtist :: Artist -> String
decodeArtist a = printf decodeArtistFormatString name num albums'
    where name = addDash $ a^.artistName
          num = a^.numListened
          albums' = intercalate "\n" (map decodeAlbum $ a^.albums)

decodeAlbum :: Album -> String
decodeAlbum a = printf decodeAlbumFormatString name date
    where name = addDash $ a^.albumName
          date = maybeDate $ a^.dateListenedTo

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
