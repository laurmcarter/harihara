{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Harihara.Lastfm
  ( module Harihara.Lastfm
  , module Harihara.Lastfm.Base
  , module Harihara.Lastfm.Config
  , module Harihara.Lastfm.Parsers
  ) where

import Control.Lens
import Network.Lastfm

import qualified Data.Text as T

import Harihara.Lastfm.Base
import Harihara.Lastfm.Config
import Harihara.Lastfm.Parsers

--------------------------------------------------------------------------------

getSimilarArtists :: Debug r d => LastfmCfg Send -> T.Text -> IO (r [ArtistResult])
getSimilarArtists cfg art = runRequest $ do
  cor <- artist_getCorrection art cfg
  artist_getSimilar (cor ^. artistName) cfg

