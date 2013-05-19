{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Harihara.Lastfm.Base where

import Control.Exception
import Control.Monad.IO.Class
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
import Harihara.Utils

data LastfmEnv = LastfmEnv
  { getApiKey   :: LfmRequest APIKey
  , signRequest :: LfmRequestAuth Ready -> LfmRequest Ready
  }

type LfmRequest = Request JSON Send
type LfmRequestAuth = Request JSON Sign

class (Functor m, Monad m, MonadIO m, MonadLog m) 
  => MonadLastfm m where
  getLastfmEnv :: m LastfmEnv

type KeyedRequest = LfmRequest (APIKey -> Ready)

--------------------------------------------------------------------------------

-- TODO: add logging
sendRequest :: (MonadLastfm m) => (Value -> Parser a) -> KeyedRequest -> m a
sendRequest prs req = do
  logDebug "Sending request to "
  key <- getApiKey <$> getLastfmEnv
  mjs <- liftIO $ lastfm $ req <*> key
  case mjs of
    Nothing -> do
      logError "No response from last.fm"
      liftIO $ throw NoResponse
    Just js -> do
      withEither (parseEither prs js)
        (liftIO . throw . ParseError)
        return

----------------------------------------

-- TODO write logging functions

{-
instance Debug (Either (Maybe Value)) (EitherT (Maybe Value)) where
  sendRequest prs req cfg = do
    res <- lift $ lastfm $ req <*> getApiKey cfg
    EitherT $ case res of
      Nothing -> do
        putStrLn "no response"
        return $ Left res
      Just js -> case parseMaybe prs js of
        Nothing -> do
          putStrLn "parse failed:"
          C8.putStrLn $ encodePretty js
          putStrLn ""
          return $ Left res
        Just a -> do
          putStrLn "parse successful:"
          print a
          putStrLn ""
          return $ Right a
  runRequest = runEitherT
-}

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.search call.
search :: (MonadLastfm m, Search a) => KeyedRequest -> m [a]
search = sendRequest parse_search

--------------------

album_search    :: (MonadLastfm m) => Text -> m [AlbumResult]
album_search  al = search $ Album.search  <*> album al

artist_search   :: (MonadLastfm m) => Text -> m [ArtistResult]
artist_search ar = search $ Artist.search <*> artist ar

tag_search      :: (MonadLastfm m) => Text -> m [TagResult]
tag_search    tg = search $ Tag.search    <*> tag tg

track_search    :: (MonadLastfm m) => Text -> m [TrackResult]
track_search  tr = search $ Track.search  <*> track tr

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getInfo call.
getInfo :: (MonadLastfm m, GetInfo a) => KeyedRequest -> m a
getInfo = sendRequest parse_getInfo

--------------------

artist_getInfo     :: (MonadLastfm m) => Text -> m ArtistResult
artist_getInfo   ar = getInfo $ Artist.getInfo <*> artist ar

tag_getInfo        :: (MonadLastfm m) => Text -> m TagResult
tag_getInfo      tg = getInfo $ Tag.getInfo    <*> tag tg

album_getInfo      :: (MonadLastfm m) => Text -> Text -> m AlbumResult
album_getInfo ar al = getInfo $ Album.getInfo  <*> artist ar <*> album al

track_getInfo      :: (MonadLastfm m) => Text -> Text -> m TrackResult
track_getInfo ar tr = getInfo $ Track.getInfo  <*> artist ar <*> track tr

----------------------------------------

album_getInfo_mbid    :: (MonadLastfm m) => Text -> m AlbumResult
album_getInfo_mbid  mb = getInfo $ Album.getInfo  <*> mbid mb

artist_getInfo_mbid   :: (MonadLastfm m) => Text -> m ArtistResult
artist_getInfo_mbid mb = getInfo $ Artist.getInfo <*> mbid mb

track_getInfo_mbid    :: (MonadLastfm m) => Text -> m TrackResult
track_getInfo_mbid  mb = getInfo $ Track.getInfo  <*> mbid mb

-- tag_getInfo_mbid does not exist, as per liblastfm

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getCorrection call.
getCorrection :: (MonadLastfm m, GetCorrection a) => KeyedRequest -> m (Maybe a)
getCorrection = sendRequest parse_getCorrection

artist_getCorrection :: (MonadLastfm m) => Text -> m (Maybe ArtistResult)
artist_getCorrection = getCorrection . (Artist.getCorrection <*>) . artist

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getSimilar call.
getSimilar :: (MonadLastfm m, GetSimilar a) => KeyedRequest -> m [a]
getSimilar = sendRequest parse_getSimilar

artist_getSimilar   :: (MonadLastfm m) => Text -> m [ArtistResult]
artist_getSimilar ar = getSimilar $ Artist.getSimilar <*> artist ar

tag_getSimilar      :: (MonadLastfm m) => Text -> m [TagResult]
tag_getSimilar    tg = getSimilar $ Tag.getSimilar    <*> tag tg

