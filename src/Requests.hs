{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Requests where

import Data.Aeson.Encode.Pretty
import Data.Aeson.Types

import Network.Lastfm
import qualified Network.Lastfm.Album as Album
import qualified Network.Lastfm.Artist as Artist
import qualified Network.Lastfm.Tag as Tag
import qualified Network.Lastfm.Track as Track

import Data.Text
import qualified Data.ByteString.Lazy.Char8 as C8

import Config
import Parsers

type SendOne t = Text -> t
type SendTwo t = Text -> Text -> t

type GetOne  d a = LastfmCfg Send -> IO (d a)
type GetMany d a = LastfmCfg Send -> IO (d [a])

type UseKey t  = Request JSON Send (APIKey -> Ready) -> t

--------------------------------------------------------------------------------

type Final = Maybe
type Debug = Either (Response JSON)

class (Functor d, Monad d) => DebugLevel d where
  sendRequest :: (Value -> Parser a) -> UseKey (GetOne d a)
  parseFail :: Response JSON -> d a

instance DebugLevel Maybe where
  sendRequest prs req cfg = do
    res <- lastfm $ req <*> getApiKey cfg
    return (parseMaybe prs =<< res)
  parseFail _ = Nothing

instance DebugLevel (Either (Maybe Value)) where
  sendRequest prs req cfg = do
    res <- lastfm $ req <*> getApiKey cfg
    let p = parseMaybe prs =<< res
    maybe (return $ Left res) (return . Right) p
  parseFail = Left

debugAnd :: (Show a, DebugLevel d) => Debug a -> (a -> IO (d b)) -> IO (d b)
debugAnd m f = case m of
  Left js -> do
    putStrLn "parse failed:"
    C8.putStrLn (encodePretty js)
    putStrLn ""
    return $ parseFail js
  Right a -> do
    putStrLn "parse successful:"
    print a
    putStrLn ""
    f a

finalAnd :: (Show a, DebugLevel d) => Final a -> (a -> IO (d b)) -> IO (d b)
finalAnd m f = case m of
  Nothing -> return $ parseFail $ error "tried to inspect an uncollected parse failure"
  Just a  -> f a

debug :: (Show a) => Debug a -> (a -> IO ()) -> IO ()
debug m f = case m of
  Left js -> do
    putStrLn "parse failed:"
    C8.putStrLn (encodePretty js)
    putStrLn ""
  Right a -> putStrLn "parse successful" >> f a

final :: (Show a) => Final a -> (a -> IO ()) -> IO ()
final m f = maybe (return ()) f m

--------------------------------------------------------------------------------

search :: (Search a, DebugLevel d) => UseKey (GetMany d a)
search = sendRequest parse_search

--------------------

album_search :: DebugLevel d => SendOne (GetMany d AlbumResult)
album_search = search . (Album.search <*>) . album

artist_search :: DebugLevel d => SendOne (GetMany d ArtistResult)
artist_search = search . (Artist.search <*>) . artist

tag_search :: DebugLevel d => SendOne (GetMany d TagResult)
tag_search = search . (Tag.search <*>) . tag

track_search :: DebugLevel d => SendOne (GetMany d TrackResult)
track_search = search . (Track.search <*>) . track

--------------------------------------------------------------------------------

getInfo :: (GetInfo a, DebugLevel d) => UseKey (GetOne d a)
getInfo = sendRequest parse_getInfo

--------------------

album_getInfo :: DebugLevel d => SendTwo (GetOne d AlbumResult)
album_getInfo art alb = getInfo $ Album.getInfo <*> artist art <*> album alb

artist_getInfo :: DebugLevel d => SendOne (GetOne d ArtistResult)
artist_getInfo = getInfo . (Artist.getInfo <*>) . artist

tag_getInfo :: DebugLevel d => SendOne (GetOne d TagResult)
tag_getInfo = getInfo . (Tag.getInfo <*>) . tag

track_getInfo :: DebugLevel d => SendTwo (GetOne d TrackResult)
track_getInfo art trk = getInfo $ Track.getInfo <*> artist art <*> track trk

----------------------------------------

album_getInfo_mbid :: DebugLevel d => SendOne (GetOne d AlbumResult)
album_getInfo_mbid = getInfo . (Album.getInfo <*>) . mbid

artist_getInfo_mbid :: DebugLevel d => SendOne (GetOne d ArtistResult)
artist_getInfo_mbid = getInfo . (Artist.getInfo <*>) . mbid

-- tag_getInfo_mbid does not exist, as per liblastfm

track_getInfo_mbid :: DebugLevel d => SendOne (GetOne d TrackResult)
track_getInfo_mbid = getInfo . (Track.getInfo <*>) . mbid

--------------------------------------------------------------------------------

getCorrection :: (GetCorrection a, DebugLevel d) => UseKey (GetOne d a)
getCorrection = sendRequest parse_getCorrection

artist_getCorrection :: DebugLevel d => SendOne (GetOne d ArtistResult)
artist_getCorrection = getCorrection . (Artist.getCorrection <*>) . artist

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

--------------------------------------------------------------------------------

getSimilar :: (GetSimilar a, DebugLevel d) => UseKey (GetMany d a)
getSimilar = sendRequest parse_getSimilar

artist_getSimilar :: DebugLevel d => SendOne (GetMany d ArtistResult)
artist_getSimilar = getSimilar . (Artist.getSimilar <*>) . artist

tag_getSimilar :: DebugLevel d => SendOne (GetMany d TagResult)
tag_getSimilar = getSimilar . (Tag.getSimilar <*>) . tag

