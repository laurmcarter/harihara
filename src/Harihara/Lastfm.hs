{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Harihara.Lastfm (
    module Harihara.Lastfm
  , module H
  ) where

import Control.Lens

import Data.Text

import Harihara.Lastfm.Base    as H
import Harihara.Lastfm.Config  as H
import Harihara.Lastfm.Parsers as H

--------------------------------------------------------------------------------

-- | Retrieve a list of artists similar to one given.
getSimilarArtists :: (MonadLastfm m) => Text -> m [ArtistResult]
getSimilarArtists ar = do
  cor <- artist_getCorrection ar
  let ar' = maybe ar (^. artistName) cor
  artist_getSimilar ar'

