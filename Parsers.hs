{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parsers 
  ( module Parsers
  , module Parsers.Types
  ) where

import Data.Aeson
import Data.Aeson.Types

import Control.Monad
import qualified Data.Text as T

import Parsers.Types
import Parsers.Extras

--------------------------------------------------------------------------------

class (FromJSON a) => GetInfo a where
  parse_getInfo :: Value -> Parser a

instance GetInfo AlbumResult where
  parse_getInfo = generic_parse_getInfo "album"

instance GetInfo ArtistResult where
  parse_getInfo = generic_parse_getInfo "artist"

instance GetInfo TagResult where
  parse_getInfo = generic_parse_getInfo "tag"

instance GetInfo TrackResult where
  parse_getInfo = generic_parse_getInfo "track"

generic_parse_getInfo :: FromJSON a => T.Text -> Value -> Parser a
generic_parse_getInfo typ =
  parseJSON >=>
  (.: typ)

--------------------------------------------------------------------------------

class (FromJSON a) => Search a where
  parse_search  :: Value -> Parser [a]

instance Search AlbumResult where
  parse_search = generic_parse_search "album"

instance Search ArtistResult where
  parse_search = generic_parse_search "artist"

instance Search TagResult where
  parse_search = generic_parse_search "tag"

instance Search TrackResult where
  parse_search = generic_parse_search "track"

generic_parse_search :: FromJSON a => T.Text -> Value -> Parser [a]
generic_parse_search typ =
  parseJSON                     >=>
  (.: "results")                >=>
  (.: (T.append typ "matches")) >=>
  (.: typ)                      >=>
  oneOrMore

--------------------------------------------------------------------------------

class (FromJSON a) => GetCorrection a where
  parse_getCorrection :: Value -> Parser a

instance GetCorrection ArtistResult where
  parse_getCorrection = generic_parse_getCorrection "artist"

generic_parse_getCorrection :: FromJSON a => T.Text -> Value -> Parser a
generic_parse_getCorrection typ =
  parseJSON          >=>
  (.: "corrections") >=>
  (.: "correction")  >=>
  (.: typ)

--------------------------------------------------------------------------------

class (FromJSON a) => GetSimilar a where
  parse_getSimilar :: Value -> Parser [a]

instance GetSimilar ArtistResult where
  parse_getSimilar = generic_parse_getSimilar "artist"

instance GetSimilar TagResult where
  parse_getSimilar = generic_parse_getSimilar "tag"

generic_parse_getSimilar :: FromJSON a => T.Text -> Value -> Parser [a]
generic_parse_getSimilar typ =
  parseJSON                           >=>
  (.: (T.concat ["similar",typ,"s"])) >=>
  (.: typ)                            >=>
  oneOrMore

