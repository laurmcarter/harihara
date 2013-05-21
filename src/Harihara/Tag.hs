
module Harihara.Tag where

import Audio.TagLib

import Control.Applicative ((<$>),(<*>))
import Control.Monad.IO.Class
import qualified Data.Text as T

import Harihara.Log

class (Functor m, Monad m, MonadIO m, MonadLog m)
  => MonadTag m where
  tagWithFiles :: [FilePath] -> TagLib a -> m (Maybe [a])
  tagWithFiles fs = liftIO . withFiles fs
  tagWithFile  :: FilePath -> TagLib a -> m (Maybe a)
  tagWithFile  f  = liftIO . withFile f

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

