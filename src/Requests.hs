{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Requests where

import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Network.Lastfm
import qualified Network.Lastfm.Album as Album
import qualified Network.Lastfm.Artist as Artist
import qualified Network.Lastfm.Tag as Tag
import qualified Network.Lastfm.Track as Track

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as C8

import Config
import Parsers

-- | denotes a @Request@ that requires one input field,
--   eg. only Album, or only Artist
type NeedOne t = Text -> t
-- | denotes a @Request@ that requires two input fields,
--   eg. Artist and Album, Artist and Track
type NeedTwo t = Text -> Text -> t

-- | denotes a @Response@ that is expected to contain
--   exactly one result.
type GetOne  d a = LastfmCfg Send -> d IO a
-- | denotes a @Response@ that is expected to contain
--   zero or more results.
type GetMany d a = LastfmCfg Send -> d IO [a]

-- | denotes a @Request@ that requires an APIKey before it is ready to send.
type UseKey t  = Request JSON Send (APIKey -> Ready) -> t

--------------------------------------------------------------------------------

-- | @MonadDebug@ describes a @MonadTrans@ and associated @Monad@ that
--   provides a certain level of debugging.
class (Monad (d IO), Monad r) => MonadDebug r d | r -> d , d -> r where
  sendRequest :: Show a => (Value -> Parser a) -> UseKey (GetOne d a)
  runRequest :: d IO a -> IO (r a)

----------------------------------------

instance MonadDebug Maybe MaybeT where
  sendRequest prs req cfg = do
    res <- lift $ lastfm $ req <*> getApiKey cfg
    MaybeT $ return (parseMaybe prs =<< res)
  runRequest = runMaybeT

-- | whenJust will instantiate a @MonadDebug@ constrained type to Maybe,
--   which does not produce any debugging printouts,
--   nor retain any unparsed data.
whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust m f = maybe (return ()) f m

----------------------------------------

instance MonadDebug (Either (Maybe Value)) (EitherT (Maybe Value)) where
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

-- | debug will instantiate a @MonadDebug@ constrained type to
--   (Either (Maybe Value)), which prints results of each call to sendRequest,
--   and returns the last unparsed response upon failure.
debug :: Either (Maybe Value) a -> (a -> IO ()) -> IO ()
debug m f = either (const $ return ()) f m

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.search call.
search :: (Search a, Show a, MonadDebug r d) => UseKey (GetMany d a)
search = sendRequest parse_search

--------------------

album_search :: (MonadDebug r d) => NeedOne (GetMany d AlbumResult)
album_search = search . (Album.search <*>) . album

artist_search :: (MonadDebug r d) => NeedOne (GetMany d ArtistResult)
artist_search = search . (Artist.search <*>) . artist

tag_search :: (MonadDebug r d) => NeedOne (GetMany d TagResult)
tag_search = search . (Tag.search <*>) . tag

track_search :: (MonadDebug r d) => NeedOne (GetMany d TrackResult)
track_search = search . (Track.search <*>) . track

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getInfo call.
getInfo :: (GetInfo a, Show a, MonadDebug r d) => UseKey (GetOne d a)
getInfo = sendRequest parse_getInfo

--------------------

album_getInfo :: (MonadDebug r d) => NeedTwo (GetOne d AlbumResult)
album_getInfo art alb = getInfo $ Album.getInfo <*> artist art <*> album alb

artist_getInfo :: (MonadDebug r d) => NeedOne (GetOne d ArtistResult)
artist_getInfo = getInfo . (Artist.getInfo <*>) . artist

tag_getInfo :: (MonadDebug r d) => NeedOne (GetOne d TagResult)
tag_getInfo = getInfo . (Tag.getInfo <*>) . tag

track_getInfo :: (MonadDebug r d) => NeedTwo (GetOne d TrackResult)
track_getInfo art trk = getInfo $ Track.getInfo <*> artist art <*> track trk

----------------------------------------

album_getInfo_mbid :: (MonadDebug r d) => NeedOne (GetOne d AlbumResult)
album_getInfo_mbid = getInfo . (Album.getInfo <*>) . mbid

artist_getInfo_mbid :: (MonadDebug r d) => NeedOne (GetOne d ArtistResult)
artist_getInfo_mbid = getInfo . (Artist.getInfo <*>) . mbid

-- tag_getInfo_mbid does not exist, as per liblastfm

track_getInfo_mbid :: (MonadDebug r d) => NeedOne (GetOne d TrackResult)
track_getInfo_mbid = getInfo . (Track.getInfo <*>) . mbid

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getCorrection call.
getCorrection :: (GetCorrection a, Show a, MonadDebug r d) => UseKey (GetOne d a)
getCorrection = sendRequest parse_getCorrection

artist_getCorrection :: (MonadDebug r d) => NeedOne (GetOne d ArtistResult)
artist_getCorrection = getCorrection . (Artist.getCorrection <*>) . artist

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

--------------------------------------------------------------------------------

-- | generic function for Last.fm's *.getSimilar call.
getSimilar :: (GetSimilar a, Show a, MonadDebug r d) => UseKey (GetMany d a)
getSimilar = sendRequest parse_getSimilar

artist_getSimilar :: (MonadDebug r d) => NeedOne (GetMany d ArtistResult)
artist_getSimilar = getSimilar . (Artist.getSimilar <*>) . artist

tag_getSimilar :: (MonadDebug r d) => NeedOne (GetMany d TagResult)
tag_getSimilar = getSimilar . (Tag.getSimilar <*>) . tag

