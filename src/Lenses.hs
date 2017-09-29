{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens (makeLenses)

import Types (FileInfo, Artist, Album)

makeLenses ''FileInfo
makeLenses ''Artist
makeLenses ''Album
