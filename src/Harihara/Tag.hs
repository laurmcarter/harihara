{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Harihara.Tag where

import Audio.TagLib
import MonadLib

import Control.Applicative ((<$>),(<*>))
import qualified Data.Text as T

import Harihara.Log

class (Monad m, BaseM m IO, MonadLog m) => MonadTag m where
  runTagLib :: TagLib a -> m a
  runTagLib = inBase . taglib

data SongInfo =  SongInfo
  { songArtist  :: !T.Text
  , songTitle   :: !T.Text
  , songAlbum   :: !T.Text
  , songComment :: !T.Text
  , songGenre   :: !T.Text
  , songYear    :: !Int
  , songTrack   :: !Int
  } deriving (Show)

getSongInfo :: FileId -> TagLib SongInfo
getSongInfo f =
  SongInfo   <$>
  getArtist  f <*>
  getTitle   f <*>
  getAlbum   f <*>
  getComment f <*>
  getGenre   f <*>
  getYear    f <*>
  getTrack   f

