
module Harihara.Tag where

import Audio.TagLib

import Control.Applicative ((<$>),(<*>))
import Control.Monad.IO.Class
import qualified Data.Text as T

import Harihara.Log

class (Functor m, Monad m, MonadIO m, MonadLog m)
  => MonadTag m where
  tagFiles :: [FilePath] -> TagLib a -> m (Maybe [a])
  tagFiles fs = liftIO . withFiles fs
  tagFile  :: FilePath -> TagLib a -> m (Maybe a)
  tagFile  f  = liftIO . withFile f

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

