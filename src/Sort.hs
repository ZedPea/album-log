module Sort
(
    sortFileInfo
)
where

import Data.List (sort)
import Control.Lens ((&), (^.), (.~))

import Types

sortFileInfo :: FileInfo -> FileInfo
sortFileInfo f = f & artists .~ sortArtists (f^.artists)
    where sortArtists = sort . map sortAlbums
          sortAlbums a = a & albums .~ sort (a^.albums)
