{-# LANGUAGE FlexibleContexts #-}

module Harihara.Tag where

import Audio.TagLib

import Control.Applicative ((<$>),(<*>))
import MonadLib
import qualified Data.Text as T

import Harihara.Log

class (Functor m, Monad m, BaseM m IO, MonadLog m)
  => MonadTag m where
  tagWithFiles :: [FilePath] -> TagLib a -> m (Maybe [a])
  tagWithFiles fs = inBase . withFiles fs
  tagWithFile  :: FilePath -> TagLib a -> m (Maybe a)
  tagWithFile  f  = inBase . withFile f

data SongInfo =  SongInfo
  { songArtist  :: !T.Text
  , songTitle   :: !T.Text
  , songAlbum   :: !T.Text
  , songComment :: !T.Text
  , songGenre   :: !T.Text
  , songYear    :: !Int
  , songTrack   :: !Int
  } deriving (Show)

getSongInfo :: TagLib SongInfo
getSongInfo  =
  SongInfo   <$>
  getArtist  <*>
  getTitle   <*>
  getAlbum   <*>
  getComment <*>
  getGenre   <*>
  getYear    <*>
  getTrack   

