{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE FlexibleContexts #-}

module Harihara.Lastfm.Requests where

import Control.Applicative
import Control.Exception
import MonadLib
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import qualified Network.Lastfm as FM
import qualified Network.Lastfm.Album as Album
import qualified Network.Lastfm.Artist as Artist
import qualified Network.Lastfm.Tag as Tag
import qualified Network.Lastfm.Track as Track
import Text.Show.Pretty hiding (Value(..))

import Data.Text
import qualified Data.ByteString.Lazy.Char8 as C8

import Harihara.Lastfm.Types
import Harihara.Lastfm.Parsers
import Harihara.Log
import Harihara.Monad

-- Request Abstractions {{{

sendRequest :: (Show a) => (Value -> Parser a) -> KeyedRequest -> Harihara a
sendRequest prs req = do
  key <- getApiKey <$> getLastfmEnv
  logInfo "Lastfm: Sending request"
  mjs <- inBase $ FM.lastfm $ req <*> key
  case mjs of
    Nothing -> do
      logError "Lastfm: No response"
      inBase $ throw NoResponse
    Just js -> do
      logInfo "Lastfm: Received response"
      let jsonStr = C8.unpack $ encodePretty js
      logDebug $ "Lastfm Response:\n" ++ jsonStr
      logInfo "Lastfm: Parsing Response"
      case parseEither prs js of
        Left err -> do
          logError $  "Lastfm: No Parse: " ++ err
          inBase $ throw $ ParseError err
        Right res -> do
          logDebug $ "Lastfm: Parse Successful:\n" ++ ppShow res
          return res

-- | generic function for Last.fm's *.search call.
search :: (Search a) => KeyedRequest -> Harihara [a]
search req = do
  logInfo "Request type 'search'"
  sendRequest parse_search req

-- | generic function for Last.fm's *.getInfo call.
getInfo :: (GetInfo a) => KeyedRequest -> Harihara a
getInfo req = do
  logInfo "Request type 'getInfo'"
  sendRequest parse_getInfo req

-- | generic function for Last.fm's *.getCorrection call.
getCorrection :: (GetCorrection a) => KeyedRequest -> Harihara (Maybe a)
getCorrection req = do
  logInfo "Request type 'getCorrection'"
  sendRequest parse_getCorrection req

-- | generic function for Last.fm's *.getSimilar call.
getSimilar :: (GetSimilar a) => KeyedRequest -> Harihara [a]
getSimilar req = do
  logInfo "Request type 'getSimilar'"
  sendRequest parse_getSimilar req

-- }}}

-- Search Requests {{{

lastfm_search_album    :: Text -> Harihara [AlbumSearch]
lastfm_search_album  al = do
  logInfo $ "Album " ++ ppShow al
  search $ Album.search  <*> FM.album al

lastfm_search_artist   :: Text -> Harihara [ArtistSearch]
lastfm_search_artist ar = do
  logInfo $ "Artist " ++ ppShow ar
  search $ Artist.search <*> FM.artist ar

lastfm_search_tag      :: Text -> Harihara [Tag]
lastfm_search_tag    tg = do
  logInfo $ "Tag " ++ ppShow tg
  search $ Tag.search    <*> FM.tag tg

lastfm_search_track    :: Text -> Harihara [TrackSearch]
lastfm_search_track  tr = do
  logInfo $ "Track " ++ ppShow tr
  search $ Track.search  <*> FM.track tr

-- }}}

-- GetInfo Requests {{{

lastfm_getInfo_artist     :: Text -> Harihara ArtistInfo
lastfm_getInfo_artist   ar = do
  logInfo $ "Artist " ++ ppShow ar
  getInfo $ Artist.getInfo <*> FM.artist ar

lastfm_getInfo_tag        :: Text -> Harihara Tag
lastfm_getInfo_tag      tg = do
  logInfo $ "Tag " ++ ppShow tg
  getInfo $ Tag.getInfo    <*> FM.tag tg

lastfm_getInfo_artist_album :: Text -> Text -> Harihara AlbumInfo
lastfm_getInfo_artist_album ar al = do
  logInfo $ "Artist " ++ ppShow ar ++ ", Album " ++ ppShow al
  getInfo $ Album.getInfo  <*> FM.artist ar <*> FM.album al

lastfm_getInfo_artist_track :: Text -> Text -> Harihara TrackInfo
lastfm_getInfo_artist_track ar tr = do
  logInfo $ "Artist " ++ ppShow ar ++ ", Track " ++ ppShow tr
  getInfo $ Track.getInfo  <*> FM.artist ar <*> FM.track tr


lastfm_getInfo_album_mbid    :: Text -> Harihara AlbumInfo
lastfm_getInfo_album_mbid  mb = do
  logInfo $ "MBID " ++ ppShow mb
  getInfo $ Album.getInfo  <*> FM.mbid mb

lastfm_getInfo_artist_mbid   :: Text -> Harihara ArtistInfo
lastfm_getInfo_artist_mbid mb = do
  logInfo $ "MBID " ++ ppShow mb
  getInfo $ Artist.getInfo <*> FM.mbid mb

lastfm_getInfo_track_mbid    :: Text -> Harihara TrackInfo
lastfm_getInfo_track_mbid  mb = do
  logInfo $ "MBID " ++ ppShow mb
  getInfo $ Track.getInfo  <*> FM.mbid mb

-- tag_getInfo_mbid does not exist, as per liblastfm

-- }}}

-- GetCorrection Requests {{{

lastfm_getCorrection_artist :: Text -> Harihara (Maybe ArtistCorrection)
lastfm_getCorrection_artist ar = do
  logInfo $ "Artist " ++ ppShow ar
  getCorrection $ Artist.getCorrection <*> FM.artist ar

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

-- }}}

-- GetSimilar Requests {{{

lastfm_getSimilar_artist   :: Text -> Harihara [ArtistSimilar]
lastfm_getSimilar_artist ar = do
  logInfo $ "Artist " ++ ppShow ar
  getSimilar $ Artist.getSimilar <*> FM.artist ar

lastfm_getSimilar_tag      :: Text -> Harihara [Tag]
lastfm_getSimilar_tag    tg = do
  logInfo $ "Tag " ++ ppShow tg
  getSimilar $ Tag.getSimilar    <*> FM.tag tg

-- }}}

