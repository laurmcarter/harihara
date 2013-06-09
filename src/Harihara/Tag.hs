
module Harihara.Tag
  ( module Harihara.Tag
  , module H
  ) where

import Control.Applicative ((<$>),(<*>))
import Data.Text

import Harihara.Tag.Types as H
import Harihara.Tag.Operations as H

data TagTrack =  TagTrack
  { songTitle   :: !Text
  , songArtist  :: !Text
  , songAlbum   :: !Text
  , songComment :: !Text
  , songGenre   :: !Text
  , songYear    :: !Int
  , songTrack   :: !Int
  } deriving (Show)

getTrackInfo :: FileId -> Tag TagTrack
getTrackInfo f =
  TagTrack     <$>
  getTitle   f <*>
  getArtist  f <*>
  getAlbum   f <*>
  getComment f <*>
  getGenre   f <*>
  getYear    f <*>
  getTrack   f 

