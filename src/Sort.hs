module Sort
(
    sortFileInfo,
    sortFileInfoPure
)
where

import Data.List (sort)
import Control.Lens ((%=), (%~), (&), traversed)

import Types (AlbumStateT, FileInfo)
import Lenses (artists, albums)

sortFileInfo :: AlbumStateT ()
sortFileInfo = do
    artists.traversed.albums %= sort
    artists %= sort

sortFileInfoPure :: FileInfo -> FileInfo
sortFileInfoPure f = f & artists %~ sortArtists
    where sortArtists = sort . map sortAlbums
          sortAlbums a = a & albums %~ sort
