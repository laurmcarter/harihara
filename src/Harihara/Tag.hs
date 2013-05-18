{-# LANGUAGE DeriveDataTypeable #-}

module Harihara.Tag (
    SongInfo(..)
  , getSongInfo
  ) where

import Audio.TagLib

import Control.Applicative ((<$>),(<*>))
import Data.Typeable (Typeable)
import qualified Control.Exception as E
import qualified Data.Text as T

import Harihara.Log

data SongInfo =  SongInfo
  { songArtist  :: !T.Text
  , songTitle   :: !T.Text
  , songAlbum   :: !T.Text
  , songComment :: !T.Text
  , songGenre   :: !T.Text
  , songYear    :: !Int
  , songTrack   :: !Int
  } deriving (Show)

getSongInfo :: FilePath -> IO SongInfo
getSongInfo path = do
  mb <- withFile path $
        SongInfo   <$>
        getArtist  <*>
        getTitle   <*>
        getAlbum   <*>
        getComment <*>
        getGenre   <*>
        getYear    <*>
        getTrack   
  case mb of
    Just info -> return info
    Nothing   -> E.throwIO (FileNotFound path)

