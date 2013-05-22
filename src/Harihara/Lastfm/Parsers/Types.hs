{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Harihara.Lastfm.Parsers.Types where

import Data.Aeson.Types

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Text

import Harihara.Lastfm.Parsers.Extras

type URL = Text

--------------------------------------------------------------------------------

data AlbumResult = AlbumResult
  { _albumName        :: !Text
  , _albumArtist      :: !Text
  , _albumId          :: !Integer
  , _albumURL         :: !URL
  , _albumImages      :: ![Image]
  , _albumMBId        :: !(Maybe Text)          -- +-in 'getInfo', not in 'search'
  , _albumReleaseDate :: !(Maybe Text)          -- |
  , _albumTracks      :: !(Maybe [TrackResult]) -- |
  , _albumTags        :: !(Maybe [TagResult])   -- +
  , _positionInAlbum  :: !(Maybe Integer)       --  NB: in 'track.getInfo'
  } deriving (Show)

instance FromJSON AlbumResult where
  parseJSON (Object r) =
      AlbumResult              <$>
      r .:       "name"        <*>
      r .:       "artist"      <*>
      r .:       "id"          <*>
      r .:       "url"         <*>
      r .:       "image"       <*>
      r .:?      "mbid"        <*>
      r .:?      "releasedate" <*>
      (r .:?     "tracks"
        >>=? (.: "track"))     <*>
      (r .:?     "toptags"
        >>=? (.: "tag"))       <*>
      r @@?      "position"
  parseJSON _ = mzero

--------------------------------------------------------------------------------

data ArtistResult = ArtistResult
  { _artistName       :: !Text
  , _artistURL        :: !URL
  , _artistMBId       :: !(Maybe Text)
  , _artistImages     :: !(Maybe [Image])
  , _artistTags       :: !(Maybe [TagResult])    -- +-in 'getInfo', not in 'search'
  , _artistSimilar    :: !(Maybe [ArtistResult]) -- +
  } deriving (Show)

instance FromJSON ArtistResult where
  parseJSON (Object r) =
      ArtistResult          <$>
      r .:       "name"     <*>
      r .:       "url"      <*>
      r .:?      "mbid"     <*>
      r .:?      "image"    <*>
      (r .:?     "tags"
        >>=? (.: "tag"))    <*>
      (r .:?     "similar"
        >>=? (.: "artist"))
  parseJSON _ = mzero

--------------------------------------------------------------------------------

data TagResult = TagResult
  { _tagName :: !Text
  , _tagURL  :: !URL
  } deriving (Show)

instance FromJSON TagResult where
  parseJSON (Object r) =
      TagResult   <$>
      r .: "name" <*>
      r .: "url"
  parseJSON _ = mzero

--------------------------------------------------------------------------------

data TrackResult = TrackResult
  { _trackName     :: !Text
  , _trackArtist   :: !(Either Text ArtistResult) -- only name in 'search',
                                                  --  name and mbid in 'getInfo'
  , _trackURL      :: !URL
  , _trackImages   :: !(Maybe [Image])     -- NB: in 'search', not in 'getInfo'
  , _trackMBId     :: !(Maybe Text)        -- +-in 'getInfo', not in 'search'
  , _trackDuration :: !(Maybe Integer)     -- |
  , _trackAlbum    :: !(Maybe AlbumResult) -- |
  , _trackRank     :: !(Maybe Integer)     -- +
  } deriving (Show)

instance FromJSON TrackResult where
  parseJSON (Object r) =
      TrackResult          <$>
      r .:  "name"         <*>
      (r .:  "artist"
        >>= couldBeEither) <*>
      r .:  "url"          <*>
      r .:  "image"        <*>
      r .:? "mbid"         <*>
      r .:? "duration"     <*>
      r .:? "album"        <*>
      r @@? "rank"
  parseJSON _ = mzero

--------------------------------------------------------------------------------

data ImageSize
  = Small
  | Medium
  | Large
  | XLarge
  | Mega
  | Other !Text
  deriving (Show)

instance FromJSON ImageSize where
  parseJSON (String r) = return $ case r of
    "small"      -> Small
    "medium"     -> Medium
    "large"      -> Large
    "extralarge" -> XLarge
    "mega"       -> Mega
    _            -> Other r
  parseJSON _ = mzero

--------------------------------------------------------------------------------

data Image = Image
  { _imageSize :: !ImageSize
  , _imageURL  :: !URL
  } deriving (Show)

instance FromJSON Image where
  parseJSON (Object r) = 
    Image        <$>
    r .: "size"  <*>
    r .: "#text"
  parseJSON _ = mzero

makeLenses ''AlbumResult
makeLenses ''ArtistResult
makeLenses ''TagResult
makeLenses ''TrackResult
makeLenses ''Image

