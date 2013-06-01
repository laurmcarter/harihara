module Harihara.DB.Tag where

import Harihara.DB.Schema
import Harihara.Lastfm.Parsers.Types

import Control.Applicative
import Data.Text hiding (map)

dbTrack :: Text -> Text -> Text -> FilePath
  -> Maybe AlbumInfo -> Maybe TrackInfo -> DBTrack
dbTrack tl art alb f mal mtr = DBTrack tl art alb f
  (mbid <$> mtr)
  (url <$> mtr)
  (map tagName <$> tags <$> mtr)
  (releaseDate <$> mal)
  (map imageURL <$> images <$> mal)

