{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Harihara.Lastfm (
    module Harihara.Lastfm
  , module Harihara.Lastfm.Base
  , module Harihara.Lastfm.Config
  , module Harihara.Lastfm.Parsers
  ) where

import Control.Lens

import Data.Text

import Harihara.Lastfm.Base
import Harihara.Lastfm.Config
import Harihara.Lastfm.Parsers

--------------------------------------------------------------------------------

getSimilarArtists :: (MonadLastfm m) => Text -> m [ArtistResult]
getSimilarArtists ar = do
  cor <- artist_getCorrection ar
  let ar' = maybe ar (^. artistName) cor
  artist_getSimilar ar'

