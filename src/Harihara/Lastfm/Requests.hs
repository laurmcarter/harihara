{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE FlexibleContexts #-}

module Harihara.Lastfm.Requests where

import Control.Applicative
import Control.Exception
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Text.Show.Pretty hiding (Value(..))

import qualified Network.Lastfm as FM
import qualified Network.Lastfm.Album as Album
import qualified Network.Lastfm.Artist as Artist
import qualified Network.Lastfm.Tag as Tag
import qualified Network.Lastfm.Track as Track

import Data.Text
import qualified Data.ByteString.Lazy.Char8 as C8

import Harihara.Lastfm.Types
import Harihara.Lastfm.Parsers
import Harihara.Log

-- Request Abstractions {{{

sendRequest :: (Show a) => (Value -> Parser a) -> KeyedRequest -> Lastfm a
sendRequest prs req = do
  key <- getsLastfmEnv getApiKey
  logInfo "Sending request"
  mjs <- io $ FM.lastfm $ req <*> key
  case mjs of
    Nothing -> do
      logError "No response"
      io $ throw NoResponse
    Just js -> do
      logInfo "Received response"
      let jsonStr = C8.unpack $ encodePretty js
      logDebug $ "Lastfm Response:\n" ++ jsonStr
      logInfo "Parsing Response"
      case parseEither prs js of
        Left err -> do
          logError $  "No Parse: " ++ err
          io $ throw $ ParseError err
        Right res -> do
          logDebug $ "Parse Successful:\n" ++ ppShow res
          return res

-- | generic function for Last.fm's *.search call.
search :: (Search a) => KeyedRequest -> Lastfm [a]
search req = do
  logInfo "Request type 'search'"
  sendRequest parse_search req

-- | generic function for Last.fm's *.getInfo call.
getInfo :: (GetInfo a) => KeyedRequest -> Lastfm a
getInfo req = do
  logInfo "Request type 'getInfo'"
  sendRequest parse_getInfo req

-- | generic function for Last.fm's *.getCorrection call.
getCorrection :: (GetCorrection a) => KeyedRequest -> Lastfm (Maybe a)
getCorrection req = do
  logInfo "Request type 'getCorrection'"
  sendRequest parse_getCorrection req

-- | generic function for Last.fm's *.getSimilar call.
getSimilar :: (GetSimilar a) => KeyedRequest -> Lastfm [a]
getSimilar req = do
  logInfo "Request type 'getSimilar'"
  sendRequest parse_getSimilar req

-- }}}

-- Search Requests {{{

search_album    :: Text -> Lastfm [AlbumSearch]
search_album  al = do
  logDebug $ "Album " ++ ppShow al
  search $ Album.search  <*> FM.album al

search_artist   :: Text -> Lastfm [ArtistSearch]
search_artist ar = do
  logDebug $ "Artist " ++ ppShow ar
  search $ Artist.search <*> FM.artist ar

search_tag      :: Text -> Lastfm [GenreTag]
search_tag    tg = do
  logDebug $ "Tag " ++ ppShow tg
  search $ Tag.search    <*> FM.tag tg

search_track    :: Text -> Lastfm [TrackSearch]
search_track  tr = do
  logDebug $ "Track " ++ ppShow tr
  search $ Track.search  <*> FM.track tr

-- }}}

-- GetInfo Requests {{{

getInfo_artist     :: Text -> Lastfm ArtistInfo
getInfo_artist   ar = do
  logDebug $ "Artist " ++ ppShow ar
  getInfo $ Artist.getInfo <*> FM.artist ar

getInfo_tag        :: Text -> Lastfm GenreTag
getInfo_tag      tg = do
  logDebug $ "Tag " ++ ppShow tg
  getInfo $ Tag.getInfo    <*> FM.tag tg

getInfo_artist_album :: Text -> Text -> Lastfm AlbumInfo
getInfo_artist_album ar al = do
  logDebug $ "Artist " ++ ppShow ar ++ ", Album " ++ ppShow al
  getInfo $ Album.getInfo  <*> FM.artist ar <*> FM.album al

getInfo_artist_track :: Text -> Text -> Lastfm TrackInfo
getInfo_artist_track ar tr = do
  logDebug $ "Artist " ++ ppShow ar ++ ", Track " ++ ppShow tr
  getInfo $ Track.getInfo  <*> FM.artist ar <*> FM.track tr


getInfo_album_mbid    :: Text -> Lastfm AlbumInfo
getInfo_album_mbid  mb = do
  logDebug $ "MBID " ++ ppShow mb
  getInfo $ Album.getInfo  <*> FM.mbid mb

getInfo_artist_mbid   :: Text -> Lastfm ArtistInfo
getInfo_artist_mbid mb = do
  logDebug $ "MBID " ++ ppShow mb
  getInfo $ Artist.getInfo <*> FM.mbid mb

getInfo_track_mbid    :: Text -> Lastfm TrackInfo
getInfo_track_mbid  mb = do
  logDebug $ "MBID " ++ ppShow mb
  getInfo $ Track.getInfo  <*> FM.mbid mb

-- tag_getInfo_mbid does not exist, as per liblastfm

-- }}}

-- GetCorrection Requests {{{

getCorrection_artist :: Text -> Lastfm (Maybe ArtistCorrection)
getCorrection_artist ar = do
  logDebug $ "Artist " ++ ppShow ar
  getCorrection $ Artist.getCorrection <*> FM.artist ar

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

-- }}}

-- GetSimilar Requests {{{

getSimilar_artist   :: Text -> Lastfm [ArtistSimilar]
getSimilar_artist ar = do
  logDebug $ "Artist " ++ ppShow ar
  getSimilar $ Artist.getSimilar <*> FM.artist ar

getSimilar_tag      :: Text -> Lastfm [GenreTag]
getSimilar_tag    tg = do
  logDebug $ "Tag " ++ ppShow tg
  getSimilar $ Tag.getSimilar    <*> FM.tag tg

-- }}}

