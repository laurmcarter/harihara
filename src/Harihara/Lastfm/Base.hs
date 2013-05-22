{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Harihara.Lastfm.Base where

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

import Harihara.Lastfm.Parsers
import Harihara.Log

data LastfmEnv = LastfmEnv
  { getApiKey   :: LfmRequest APIKey
  , signRequest :: LfmRequestAuth Ready -> LfmRequest Ready
  }

type LfmRequest = Request JSON Send
type LfmRequestAuth = Request JSON Sign

class (Functor m, Monad m, BaseM m IO, MonadLog m) 
  => MonadLastfm m where
  getLastfmEnv :: m LastfmEnv

type KeyedRequest = LfmRequest (APIKey -> Ready)

--------------------------------------------------------------------------------

sendRequest :: (Show a, MonadLastfm m) => (Value -> Parser a) -> KeyedRequest -> m a
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

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.search call.
search :: (MonadLastfm m, Search a) => KeyedRequest -> m [a]
search req = do
  logInfo "Request type 'search'"
  sendRequest parse_search req

--------------------

album_search    :: (MonadLastfm m) => Text -> m [AlbumResult]
album_search  al = do
  logInfo $ "Album " ++ show al
  search $ Album.search  <*> album al

artist_search   :: (MonadLastfm m) => Text -> m [ArtistResult]
artist_search ar = do
  logInfo $ "Artist " ++ show ar
  search $ Artist.search <*> artist ar

tag_search      :: (MonadLastfm m) => Text -> m [TagResult]
tag_search    tg = do
  logInfo $ "Tag " ++ show tg
  search $ Tag.search    <*> tag tg

track_search    :: (MonadLastfm m) => Text -> m [TrackResult]
track_search  tr = do
  logInfo $ "Track " ++ show tr
  search $ Track.search  <*> track tr

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getInfo call.
getInfo :: (MonadLastfm m, GetInfo a) => KeyedRequest -> m a
getInfo req = do
  logInfo "Request type 'getInfo'"
  sendRequest parse_getInfo req

--------------------

artist_getInfo     :: (MonadLastfm m) => Text -> m ArtistResult
artist_getInfo   ar = do
  logInfo $ "Artist " ++ show ar
  getInfo $ Artist.getInfo <*> artist ar

tag_getInfo        :: (MonadLastfm m) => Text -> m TagResult
tag_getInfo      tg = do
  logInfo $ "Tag " ++ show tg
  getInfo $ Tag.getInfo    <*> tag tg

album_getInfo      :: (MonadLastfm m) => Text -> Text -> m AlbumResult
album_getInfo ar al = do
  logInfo $ "Artist " ++ show ar ++ ", Album " ++ show al
  getInfo $ Album.getInfo  <*> artist ar <*> album al

track_getInfo      :: (MonadLastfm m) => Text -> Text -> m TrackResult
track_getInfo ar tr = do
  logInfo $ "Artist " ++ show ar ++ ", Track " ++ show tr
  getInfo $ Track.getInfo  <*> artist ar <*> track tr

----------------------------------------

album_getInfo_mbid    :: (MonadLastfm m) => Text -> m AlbumResult
album_getInfo_mbid  mb = do
  logInfo $ "MBID " ++ show mb
  getInfo $ Album.getInfo  <*> mbid mb

artist_getInfo_mbid   :: (MonadLastfm m) => Text -> m ArtistResult
artist_getInfo_mbid mb = do
  logInfo $ "MBID " ++ show mb
  getInfo $ Artist.getInfo <*> mbid mb

track_getInfo_mbid    :: (MonadLastfm m) => Text -> m TrackResult
track_getInfo_mbid  mb = do
  logInfo $ "MBID " ++ show mb
  getInfo $ Track.getInfo  <*> mbid mb

-- tag_getInfo_mbid does not exist, as per liblastfm

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getCorrection call.
getCorrection :: (MonadLastfm m, GetCorrection a) => KeyedRequest -> m (Maybe a)
getCorrection req = do
  logInfo "Request type 'getCorrection'"
  sendRequest parse_getCorrection req

artist_getCorrection :: (MonadLastfm m) => Text -> m (Maybe ArtistResult)
artist_getCorrection ar = do
  logInfo $ "Artist " ++ show ar
  getCorrection $ Artist.getCorrection <*> artist ar

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getSimilar call.
getSimilar :: (MonadLastfm m, GetSimilar a) => KeyedRequest -> m [a]
getSimilar req = do
  logInfo "Request type 'getSimilar'"
  sendRequest parse_getSimilar req

artist_getSimilar   :: (MonadLastfm m) => Text -> m [ArtistResult]
artist_getSimilar ar = do
  logInfo $ "Artist " ++ show ar
  getSimilar $ Artist.getSimilar <*> artist ar

tag_getSimilar      :: (MonadLastfm m) => Text -> m [TagResult]
tag_getSimilar    tg = do
  logInfo $ "Tag " ++ show tg
  getSimilar $ Tag.getSimilar    <*> tag tg

