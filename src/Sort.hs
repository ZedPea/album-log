module Sort
(
    sortFileInfo
)
where

import Data.List (sort)

import Types

sortFileInfo :: FileInfo -> FileInfo
sortFileInfo f@(FileInfo _ _ a) = f { artists = sortArtists a }
    where sortArtists = sort . map sortAlbums
          sortAlbums a'@(Artist _ _ a'') = a' { albums = sort a'' }
