{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Harihara.Tag where

import Audio.TagLib

import Control.Applicative ((<$>),(<*>))
import qualified Data.Text as T

data TagTrack =  TagTrack
  { songTitle   :: !T.Text
  , songArtist  :: !T.Text
  , songAlbum   :: !T.Text
  , songComment :: !T.Text
  , songGenre   :: !T.Text
  , songYear    :: !Int
  , songTrack   :: !Int
  } deriving (Show)

getTrackInfo :: FileId -> TagLib TagTrack
getTrackInfo f =
  TagTrack    <$>
  getTitle   f <*>
  getArtist  f <*>
  getAlbum   f <*>
  getComment f <*>
  getGenre   f <*>
  getYear    f <*>
  getTrack   f

