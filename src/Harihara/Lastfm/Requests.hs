{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE FlexibleContexts #-}

module Harihara.Lastfm.Requests where

import Control.Exception
import MonadLib
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Network.Lastfm
import qualified Network.Lastfm.Album as Album
import qualified Network.Lastfm.Artist as Artist
import qualified Network.Lastfm.Tag as Tag
import qualified Network.Lastfm.Track as Track

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
  mjs <- inBase $ lastfm $ req <*> key
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
          logDebug $ "Lastfm: Parse Successful:\n" ++ show res
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

lastfm_search_album    :: Text -> Harihara [AlbumResult]
lastfm_search_album  al = do
  logInfo $ "Album " ++ show al
  search $ Album.search  <*> album al

lastfm_search_artist   :: Text -> Harihara [ArtistResult]
lastfm_search_artist ar = do
  logInfo $ "Artist " ++ show ar
  search $ Artist.search <*> artist ar

lastfm_search_tag      :: Text -> Harihara [TagResult]
lastfm_search_tag    tg = do
  logInfo $ "Tag " ++ show tg
  search $ Tag.search    <*> tag tg

lastfm_search_track    :: Text -> Harihara [TrackResult]
lastfm_search_track  tr = do
  logInfo $ "Track " ++ show tr
  search $ Track.search  <*> track tr

-- }}}

-- GetInfo Requests {{{

lastfm_getInfo_artist     :: Text -> Harihara ArtistResult
lastfm_getInfo_artist   ar = do
  logInfo $ "Artist " ++ show ar
  getInfo $ Artist.getInfo <*> artist ar

lastfm_getInfo_tag        :: Text -> Harihara TagResult
lastfm_getInfo_tag      tg = do
  logInfo $ "Tag " ++ show tg
  getInfo $ Tag.getInfo    <*> tag tg

lastfm_getInfo_artist_album :: Text -> Text -> Harihara AlbumResult
lastfm_getInfo_artist_album ar al = do
  logInfo $ "Artist " ++ show ar ++ ", Album " ++ show al
  getInfo $ Album.getInfo  <*> artist ar <*> album al

lastfm_getInfo_artist_track :: Text -> Text -> Harihara TrackResult
lastfm_getInfo_artist_track ar tr = do
  logInfo $ "Artist " ++ show ar ++ ", Track " ++ show tr
  getInfo $ Track.getInfo  <*> artist ar <*> track tr


lastfm_getInfo_album_mbid    :: Text -> Harihara AlbumResult
lastfm_getInfo_album_mbid  mb = do
  logInfo $ "MBID " ++ show mb
  getInfo $ Album.getInfo  <*> mbid mb

lastfm_getInfo_artist_mbid   :: Text -> Harihara ArtistResult
lastfm_getInfo_artist_mbid mb = do
  logInfo $ "MBID " ++ show mb
  getInfo $ Artist.getInfo <*> mbid mb

lastfm_getInfo_track_mbid    :: Text -> Harihara TrackResult
lastfm_getInfo_track_mbid  mb = do
  logInfo $ "MBID " ++ show mb
  getInfo $ Track.getInfo  <*> mbid mb

-- tag_getInfo_mbid does not exist, as per liblastfm

-- }}}

-- GetCorrection Requests {{{

lastfm_getCorrection_artist :: Text -> Harihara (Maybe ArtistResult)
lastfm_getCorrection_artist ar = do
  logInfo $ "Artist " ++ show ar
  getCorrection $ Artist.getCorrection <*> artist ar

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

-- }}}

-- GetSimilar Requests {{{

lastfm_getSimilar_artist   :: Text -> Harihara [ArtistResult]
lastfm_getSimilar_artist ar = do
  logInfo $ "Artist " ++ show ar
  getSimilar $ Artist.getSimilar <*> artist ar

lastfm_getSimilar_tag      :: Text -> Harihara [TagResult]
lastfm_getSimilar_tag    tg = do
  logInfo $ "Tag " ++ show tg
  getSimilar $ Tag.getSimilar    <*> tag tg

-- }}}

