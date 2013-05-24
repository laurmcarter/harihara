{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Harihara.Lastfm.Parsers.Types where

import Data.Aeson.Types

import Control.Applicative
import Control.Monad
import Data.Text

import Harihara.Lastfm.Parsers.Extras

type URL = Text

--------------------------------------------------------------------------------

data AlbumResult = AlbumResult
  { albumName        :: !Text
  , albumArtist      :: !Text
  , albumId          :: !Integer
  , albumURL         :: !URL
  , albumImages      :: ![Image]
  , albumMBId        :: !(Maybe Text)          -- +-in 'getInfo', not in 'search'
  , albumReleaseDate :: !(Maybe Text)          -- |
  , albumTracks      :: !(Maybe [TrackResult]) -- |
  , albumTags        :: !(Maybe [TagResult])   -- +
  , positionInAlbum  :: !(Maybe Integer)       --  NB: in 'track.getInfo'
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
  { artistName       :: !Text
  , artistURL        :: !URL
  , artistMBId       :: !(Maybe Text)
  , artistImages     :: !(Maybe [Image])
  , artistTags       :: !(Maybe [TagResult])    -- +-in 'getInfo', not in 'search'
  , artistSimilar    :: !(Maybe [ArtistResult]) -- +
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
  { tagName :: !Text
  , tagURL  :: !URL
  } deriving (Show)

instance FromJSON TagResult where
  parseJSON (Object r) =
      TagResult   <$>
      r .: "name" <*>
      r .: "url"
  parseJSON _ = mzero

--------------------------------------------------------------------------------

data TrackResult = TrackResult
  { trackName     :: !Text
  , trackArtist   :: !(Either Text ArtistResult) -- only name in 'search',
                                                 --  name and mbid in 'getInfo'
  , trackURL      :: !URL
  , trackImages   :: !(Maybe [Image])     -- NB: in 'search', not in 'getInfo'
  , trackMBId     :: !(Maybe Text)        -- +-in 'getInfo', not in 'search'
  , trackDuration :: !(Maybe Integer)     -- |
  , trackAlbum    :: !(Maybe AlbumResult) -- |
  , trackRank     :: !(Maybe Integer)     -- +
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
  { imageSize :: !ImageSize
  , imageURL  :: !URL
  } deriving (Show)

instance FromJSON Image where
  parseJSON (Object r) = 
    Image        <$>
    r .: "size"  <*>
    r .: "#text"
  parseJSON _ = mzero

