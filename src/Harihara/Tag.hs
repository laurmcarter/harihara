{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Harihara.Tag where

import Audio.TagLib

import Control.Applicative ((<$>),(<*>))
import qualified Data.Text as T

data SongInfo =  SongInfo
  { songTitle   :: !T.Text
  , songArtist  :: !T.Text
  , songAlbum   :: !T.Text
  , songComment :: !T.Text
  , songGenre   :: !T.Text
  , songYear    :: !Int
  , songTrack   :: !Int
  } deriving (Show)

getSongInfo :: FileId -> TagLib SongInfo
getSongInfo f =
  SongInfo     <$>
  getTitle   f <*>
  getArtist  f <*>
  getAlbum   f <*>
  getComment f <*>
  getGenre   f <*>
  getYear    f <*>
  getTrack   f

