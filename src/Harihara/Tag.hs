{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Harihara.Tag where

import Audio.TagLib
import MonadLib

import Control.Applicative ((<$>),(<*>))
import Data.List (intercalate)
import qualified Data.Text as T

import Harihara.Log

class (Functor m, Monad m, BaseM m IO, MonadLog m)
  => MonadTag m where
  tagWithFiles :: [FilePath] -> TagLib a -> m (Maybe [a])
  tagWithFiles fs m = do
    logInfo "Tag: Interacting with files"
    logDebug $ "Files: " ++ intercalate ", " (map show fs)
    inBase $ withFiles fs m
  tagWithFile  :: FilePath -> TagLib a -> m (Maybe a)
  tagWithFile  f  m = do
    logInfo "Tag: Interacting with file"
    logDebug $ "File: " ++ show f
    inBase $ withFile f m

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

