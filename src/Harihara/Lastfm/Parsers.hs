{-# LANGUAGE OverloadedStrings #-}

module Harihara.Lastfm.Parsers 
  ( module Harihara.Lastfm.Parsers
  , module Harihara.Lastfm.Parsers.Types
  ) where

import Data.Aeson
import Data.Aeson.Types

import Control.Monad
import qualified Data.Text as T

import Harihara.Lastfm.Parsers.Types
import Harihara.Lastfm.Parsers.Extras

--------------------------------------------------------------------------------

generic_parse_getInfo :: FromJSON a => T.Text -> Value -> Parser a
generic_parse_getInfo typ =
  parseJSON >=>
  (.: typ)

class (Show a, FromJSON a) => GetInfo a where
  parse_getInfo :: Value -> Parser a

instance GetInfo AlbumInfo where
  parse_getInfo = generic_parse_getInfo "album"

instance GetInfo ArtistInfo where
  parse_getInfo = generic_parse_getInfo "artist"

instance GetInfo Tag where
  parse_getInfo = generic_parse_getInfo "tag"

instance GetInfo TrackInfo where
  parse_getInfo = generic_parse_getInfo "track"

--------------------------------------------------------------------------------

generic_parse_search :: (FromJSON a) => T.Text -> Value -> Parser [a]
generic_parse_search typ =
  parseJSON                     >=>
  (.: "results")                >=>
  (.: (T.append typ "matches")) >=>
  (.: typ)                      >=>
  oneOrMore

class (Show a, FromJSON a) => Search a where
  parse_search  :: Value -> Parser [a]

instance Search AlbumSearch where
  parse_search = generic_parse_search "album"

instance Search ArtistSearch where
  parse_search = generic_parse_search "artist"

instance Search Tag where
  parse_search = generic_parse_search "tag"

instance Search TrackSearch where
  parse_search = generic_parse_search "track"

--------------------------------------------------------------------------------

generic_parse_getCorrection :: FromJSON a => T.Text -> Value -> Parser (Maybe a)
generic_parse_getCorrection typ (Object o) = do
  eo <- o .: "corrections" >>= couldBeEither
    :: Parser (Either String Object)
  case eo of
    Left _ -> return Nothing
    Right o' -> fmap Just $ do
      r <- o' .: "correction"
      r .: typ
generic_parse_getCorrection _ _ = mzero

class (Show a, FromJSON a) => GetCorrection a where
  parse_getCorrection :: Value -> Parser (Maybe a)

instance GetCorrection ArtistCorrection where
  parse_getCorrection = generic_parse_getCorrection "artist"

--------------------------------------------------------------------------------

generic_parse_getSimilar :: FromJSON a => T.Text -> Value -> Parser [a]
generic_parse_getSimilar typ =
  parseJSON                         >=>
  (.: T.concat ["similar",typ,"s"]) >=>
  (.: typ)                          >=>
  oneOrMore

class (Show a, FromJSON a) => GetSimilar a where
  parse_getSimilar :: Value -> Parser [a]

instance GetSimilar ArtistSimilar where
  parse_getSimilar = generic_parse_getSimilar "artist"

instance GetSimilar Tag where
  parse_getSimilar = generic_parse_getSimilar "tag"

