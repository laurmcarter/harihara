{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

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

type GetOne  m a = LastfmCfg Send -> IO (m a)
type GetMany m a = LastfmCfg Send -> IO (m [a])

type UseKey t  = Request JSON Send (APIKey -> Ready) -> t

--------------------------------------------------------------------------------

class Debug d where
  sendRequest :: (Value -> Parser a) -> UseKey (GetOne d a)

instance Debug Maybe where
  sendRequest prs req cfg = do
    res <- lastfm $ req <*> getApiKey cfg
    return (parseMaybe prs =<< res)

instance Debug (Either (Maybe Value)) where
  sendRequest prs req cfg = do
    res <- lastfm $ req <*> getApiKey cfg
    let p = parseMaybe prs =<< res
    maybe (return $ Left res) (return . Right) p

debug :: Either (Maybe Value) a -> (a -> IO ()) -> IO ()
debug e f = either (C8.putStrLn . encodePretty) f e

final :: Show a => Maybe a -> (a -> IO ()) -> IO ()
final m f = maybe (return ()) f m

--------------------------------------------------------------------------------

search :: (Search a, Debug d) => UseKey (GetMany d a)
search = sendRequest parse_search

--------------------

album_search :: Debug d => SendOne (GetMany d AlbumResult)
album_search = search . (Album.search <*>) . album

artist_search :: Debug d => SendOne (GetMany d ArtistResult)
artist_search = search . (Artist.search <*>) . artist

tag_search :: Debug d => SendOne (GetMany d TagResult)
tag_search = search . (Tag.search <*>) . tag

track_search :: Debug d => SendOne (GetMany d TrackResult)
track_search = search . (Track.search <*>) . track

--------------------------------------------------------------------------------

getInfo :: (GetInfo a, Debug d) => UseKey (GetOne d a)
getInfo = sendRequest parse_getInfo

--------------------

album_getInfo :: Debug d => SendTwo (GetOne d AlbumResult)
album_getInfo art alb = getInfo $ Album.getInfo <*> artist art <*> album alb

artist_getInfo :: Debug d => SendOne (GetOne d ArtistResult)
artist_getInfo = getInfo . (Artist.getInfo <*>) . artist

tag_getInfo :: Debug d => SendOne (GetOne d TagResult)
tag_getInfo = getInfo . (Tag.getInfo <*>) . tag

track_getInfo :: Debug d => SendTwo (GetOne d TrackResult)
track_getInfo art trk = getInfo $ Track.getInfo <*> artist art <*> track trk

----------------------------------------

album_getInfo_mbid :: Debug d => SendOne (GetOne d AlbumResult)
album_getInfo_mbid = getInfo . (Album.getInfo <*>) . mbid

artist_getInfo_mbid :: Debug d => SendOne (GetOne d ArtistResult)
artist_getInfo_mbid = getInfo . (Artist.getInfo <*>) . mbid

-- tag_getInfo_mbid does not exist, as per liblastfm

track_getInfo_mbid :: Debug d => SendOne (GetOne d TrackResult)
track_getInfo_mbid = getInfo . (Track.getInfo <*>) . mbid

--------------------------------------------------------------------------------

getCorrection :: (GetCorrection a, Debug d) => UseKey (GetOne d a)
getCorrection = sendRequest parse_getCorrection

artist_getCorrection :: Debug d => SendOne (GetOne d ArtistResult)
artist_getCorrection = getCorrection . (Artist.getCorrection <*>) . artist

-- track_getCorrection does not exist, b/c last.FM responses seem to be useless

--------------------------------------------------------------------------------

getSimilar :: (GetSimilar a, Debug d) => UseKey (GetMany d a)
getSimilar = sendRequest parse_getSimilar

artist_getSimilar :: Debug d => SendOne (GetMany d ArtistResult)
artist_getSimilar = getSimilar . (Artist.getSimilar <*>) . artist

tag_getSimilar :: Debug d => SendOne (GetMany d TagResult)
tag_getSimilar = getSimilar . (Tag.getSimilar <*>) . tag

