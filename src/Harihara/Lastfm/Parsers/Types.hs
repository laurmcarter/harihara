{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Harihara.Lastfm.Parsers.Types where

import Data.Aeson.Types

import Control.Applicative
import Control.Monad
import Data.Text

import Harihara.Lastfm.Parsers.Extras

type Name = Text
type ArtistName = Text
type AlbumName = Text
type URL = Text
type MBID = Text
type Match = Double
type ReleaseDate = Text
type Rank = Int
type Duration = Int

class HasName a where
  name :: a -> Name

class HasArtist a ar | a -> ar where
  artist :: a -> ar

class HasAlbum a al | a -> al where
  album :: a -> al

class HasMBID a where
  mbid :: a -> MBID

class HasURL a where
  url :: a -> URL

class HasImages a where
  images :: a -> [Image]

class HasTags a where
  tags :: a -> [Tag]

class HasMatch a where
  match :: a -> Match

-- GetInfo {{{

data AlbumInfo = AlbumInfo
  { albumInfoName     :: Name
  , albumInfoArtist   :: ArtistName
  , albumInfoMBID     :: MBID
  , albumInfoURL      :: URL
  , albumInfoRelease  :: ReleaseDate
  , albumInfoImages   :: [Image]
  , albumInfoTopTags  :: [Tag]
  , albumInfoTracks   :: [AlbumInfoTrack]
  } deriving (Show)

instance HasName AlbumInfo where
  name = albumInfoName

instance HasArtist AlbumInfo Text where
  artist = albumInfoArtist

instance HasMBID AlbumInfo where
  mbid = albumInfoMBID

instance HasURL AlbumInfo where
  url = albumInfoURL

instance HasImages AlbumInfo where
  images = albumInfoImages

instance HasTags AlbumInfo where
  tags = albumInfoTopTags

releaseDate :: AlbumInfo -> ReleaseDate
releaseDate = albumInfoRelease

tracks :: AlbumInfo -> [AlbumInfoTrack]
tracks = albumInfoTracks

instance FromJSON AlbumInfo where
  parseJSON (Object r) =
    AlbumInfo                <$>
    r .:  "name"             <*>
    r .:  "artist"           <*>
    r .:  "mbid"             <*>
    r .:  "url"              <*>
    r .:  "releasedate"      <*>
    r .:: ("images","image") <*>
    r .:: ("toptags","tag")  <*>
    r .:: ("tracks","track")
  parseJSON _ = mzero

data AlbumInfoTrack = AlbumInfoTrack
  { albumInfoTrackName     :: Name
  , albumInfoTrackRank     :: Rank
  , albumInfoTrackDuration :: Duration
  , albumInfoTrackArtist   :: ArtistName
  } deriving (Show)

instance HasName AlbumInfoTrack where
  name = albumInfoTrackName

rank :: AlbumInfoTrack -> Rank
rank = albumInfoTrackRank

duration :: AlbumInfoTrack -> Duration
duration = albumInfoTrackDuration

instance HasArtist AlbumInfoTrack Text where
  artist = albumInfoTrackArtist

instance FromJSON AlbumInfoTrack where
  parseJSON (Object r) =
    AlbumInfoTrack   <$>
    r .:  "name"     <*>
    r @@  "rank"     <*>
    r .:# "duration" <*>
    r .:  "artist"
  parseJSON _ = mzero

--------

data ArtistInfo = ArtistInfo
  { artistInfoName    :: Text
  , artistInfoMBID    :: MBID
  , artistInfoImages  :: [Image]
  , artistInfoSimilar :: [ArtistInfoArtist]
  , artistInfoTags    :: [Tag]
  } deriving (Show)

instance HasName ArtistInfo where
  name = artistInfoName

instance HasMBID ArtistInfo where
  mbid = artistInfoMBID

instance HasImages ArtistInfo where
  images = artistInfoImages

similarArtists :: ArtistInfo -> [ArtistInfoArtist]
similarArtists = artistInfoSimilar

instance HasTags ArtistInfo where
  tags = artistInfoTags

instance FromJSON ArtistInfo where
  parseJSON (Object r) =
    ArtistInfo                 <$>
    r .:  "name"               <*>
    r .:  "mbid"               <*>
    r .:: ("images","image")   <*>
    r .:: ("similar","artist") <*>
    r .:: ("tags","tag")
  parseJSON _ = mzero

data ArtistInfoArtist = ArtistInfoArtist
  { artistInfoArtistName    :: Text
  , artistInfoArtistMBID    :: MBID
  , artistInfoArtistImages  :: [Image]
  } deriving (Show)

instance HasName ArtistInfoArtist where
  name = artistInfoArtistName

instance HasMBID ArtistInfoArtist where
  mbid = artistInfoArtistMBID

instance HasImages ArtistInfoArtist where
  images = artistInfoArtistImages

instance FromJSON ArtistInfoArtist where
  parseJSON (Object r) =
    ArtistInfoArtist <$>
    r .:  "name"     <*>
    r .:  "mbid"     <*>
    r .:: ("images","image")
  parseJSON _ = mzero

--------

data TrackInfo = TrackInfo
    { trackInfoName    :: Text
    , trackInfoMBID    :: MBID
    , trackInfoURL     :: URL
    , trackInfoArtist  :: TrackInfoArtist
    , trackInfoAlbum   :: TrackInfoAlbum
    , trackInfoTopTags :: [Tag]
    } deriving (Show)

instance HasName TrackInfo where
  name = trackInfoName

instance HasMBID TrackInfo where
  mbid = trackInfoMBID

instance HasURL TrackInfo where
  url = trackInfoURL

instance HasArtist TrackInfo TrackInfoArtist where
  artist = trackInfoArtist

instance HasAlbum TrackInfo TrackInfoAlbum where
  album = trackInfoAlbum

instance HasTags TrackInfo where
  tags = trackInfoTopTags

instance FromJSON TrackInfo where
  parseJSON (Object r) =
    TrackInfo      <$>
    r .:  "name"   <*>
    r .:  "mbid"   <*>
    r .:  "url"    <*>
    r .:  "artist" <*>
    r .:  "album"  <*>
    r .:: ("toptags","tag")
  parseJSON _ = mzero

data TrackInfoArtist = TrackInfoArtist
  { trackInfoArtistName :: Name
  , trackInfoArtistMBID :: MBID
  , trackInfoArtistURL  :: URL
  } deriving (Show)

instance HasName TrackInfoArtist where
  name = trackInfoArtistName

instance HasMBID TrackInfoArtist where
  mbid = trackInfoArtistMBID

instance HasURL TrackInfoArtist where
  url = trackInfoArtistURL

instance FromJSON TrackInfoArtist where
  parseJSON (Object r) =
    TrackInfoArtist <$>
    r .:  "name"    <*>
    r .:  "mbid"    <*>
    r .:  "url"
  parseJSON _ = mzero

data TrackInfoAlbum = TrackInfoAlbum 
  { trackInfoAlbumArtist :: ArtistName
  , trackInfoAlbumName   :: AlbumName
  , trackInfoAlbumMBID   :: MBID
  , trackInfoAlbumURL    :: URL
  } deriving (Show)

instance HasArtist TrackInfoAlbum Text where
  artist = trackInfoAlbumArtist

instance HasAlbum TrackInfoAlbum Text where
  album = trackInfoAlbumName

instance HasMBID TrackInfoAlbum where
  mbid = trackInfoAlbumMBID

instance HasURL TrackInfoAlbum where
  url = trackInfoAlbumURL

instance FromJSON TrackInfoAlbum where
  parseJSON (Object r) =
    TrackInfoAlbum <$>
    r .:  "artist" <*>
    r .:  "title"  <*>
    r .:  "mbid"   <*>
    r .:  "url"
  parseJSON _ = mzero

-- }}}

-- Search {{{

data AlbumSearch = AlbumSearch
  { albumSearchName   :: Name
  , albumSearchArtist :: ArtistName
  , albumSearchURL    :: URL
  , albumSearchImages :: [Image]
  } deriving (Show)

instance HasName AlbumSearch where
  name = albumSearchName

instance HasArtist AlbumSearch Text where
  artist = albumSearchArtist

instance HasURL AlbumSearch where
  url = albumSearchURL

instance HasImages AlbumSearch where
  images = albumSearchImages

instance FromJSON AlbumSearch where
  parseJSON (Object r) =
    AlbumSearch    <$>
    r .:  "name"   <*>
    r .:  "artist" <*>
    r .:  "url"    <*>
    r .:: ("images","image")
  parseJSON _ = mzero

data ArtistSearch = ArtistSearch
  { artistSearchName   :: Name
  , artistSearchMBID   :: MBID
  , artistSearchURL    :: URL
  , artistSearchImages :: [Image]
  } deriving (Show)

instance HasName ArtistSearch where
  name = artistSearchName

instance HasMBID ArtistSearch where
  mbid = artistSearchMBID

instance HasURL ArtistSearch where
  url = artistSearchURL

instance HasImages ArtistSearch where
  images = artistSearchImages

instance FromJSON ArtistSearch where
  parseJSON (Object r) =
    ArtistSearch <$>
    r .:  "name" <*>
    r .:  "mbid" <*>
    r .:  "url"  <*>
    r .:: ("images","image")
  parseJSON _ = mzero

data TrackSearch = TrackSearch
  { trackSearchName   :: Name
  , trackSearchArtist :: ArtistName
  , trackSearchURL    :: URL
  , trackSearchImages :: [Image]
  } deriving (Show)

instance HasName TrackSearch where
  name = trackSearchName

instance HasArtist TrackSearch Text where
  artist = trackSearchArtist

instance HasURL TrackSearch where
  url = trackSearchURL

instance HasImages TrackSearch where
  images = trackSearchImages

instance FromJSON TrackSearch where
  parseJSON (Object r) =
    TrackSearch    <$>
    r .:  "name"   <*>
    r .:  "artist" <*>
    r .:  "url"    <*>
    r .:: ("images","image")
  parseJSON _ = mzero

-- }}}

-- GetSimilar {{{

data ArtistSimilar = ArtistSimilar
  { artistSimilarName  :: Text
  , artistSimilarMBID  :: MBID
  , artistSimilarMatch :: Match
  , artistSimilarURL   :: URL
  } deriving (Show)

instance HasName ArtistSimilar where
  name = artistSimilarName

instance HasMBID ArtistSimilar where
  mbid = artistSimilarMBID

instance HasMatch ArtistSimilar where
  match = artistSimilarMatch

instance HasURL ArtistSimilar where
  url = artistSimilarURL

instance FromJSON ArtistSimilar where
  parseJSON (Object r) =
    ArtistSimilar <$>
    r .:  "name"  <*>
    r .:  "mbid"  <*>
    r .:# "match" <*>
    r .:  "url"
  parseJSON _ = mzero

data TrackSimilar = TrackSimilar
  { trackSimilarName :: Text
  , trackSimilarMatch :: Match
  , trackSimilarArtist :: TrackSimilarArtist
  } deriving (Show)

instance HasName TrackSimilar where
  name = trackSimilarName

instance HasMatch TrackSimilar where
  match = trackSimilarMatch

instance HasArtist TrackSimilar TrackSimilarArtist where
  artist = trackSimilarArtist

instance FromJSON TrackSimilar where
  parseJSON (Object r) =
    TrackSimilar <$>
    r .:  "name"  <*>
    r .:# "match" <*>
    r .:  "artist"
  parseJSON _ = mzero

data TrackSimilarArtist = TrackSimilarArtist 
  { trackSimilarArtistName :: Text
  , trackSimilarArtistMBID :: Text
  , trackSimilarArtistURL  :: URL
  } deriving (Show)

instance HasName TrackSimilarArtist where
  name = trackSimilarArtistName

instance HasMBID TrackSimilarArtist where
  mbid = trackSimilarArtistMBID

instance HasURL TrackSimilarArtist where
  url = trackSimilarArtistURL

instance FromJSON TrackSimilarArtist where
  parseJSON (Object r) =
    TrackSimilarArtist <$>
    r .:  "name"       <*>
    r .:  "mbid"       <*>
    r .:  "url"
  parseJSON _ = mzero

-- }}}

-- GetCorrection {{{

data ArtistCorrection = ArtistCorrection 
  { artistCorrectionName :: Text
  , artistCorrectionMBID :: MBID
  , artistCorrectionURL  :: URL
  } deriving (Show)

instance HasName ArtistCorrection where
  name = artistCorrectionName

instance HasMBID ArtistCorrection where
  mbid = artistCorrectionMBID

instance HasURL ArtistCorrection where
  url = artistCorrectionURL

instance FromJSON ArtistCorrection where
  parseJSON (Object r) =
    ArtistCorrection <$>
    r .:  "name"     <*>
    r .:  "mbid"     <*>
    r .:  "url"
  parseJSON _ = mzero

data TrackCorrection = TrackCorrection
  { trackCorrectionName :: Text
  , trackCorrectionURL  :: URL
  , trackCorrectionArtist :: ArtistCorrection
  } deriving (Show)

instance HasName TrackCorrection where
  name = trackCorrectionName

instance HasURL TrackCorrection where
  url = trackCorrectionURL

instance HasArtist TrackCorrection ArtistCorrection where
  artist = trackCorrectionArtist

instance FromJSON TrackCorrection where
  parseJSON (Object r) =
    TrackCorrection <$>
    r .:  "name"    <*>
    r .:  "mbid"    <*>
    r .:  "url"
  parseJSON _ = mzero

-- }}}

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

data Tag = Tag
  { tagName :: !Text
  , tagURL  :: !URL
  } deriving (Show)

instance FromJSON Tag where
  parseJSON (Object r) =
      Tag   <$>
      r .: "name" <*>
      r .: "url"
  parseJSON _ = mzero

