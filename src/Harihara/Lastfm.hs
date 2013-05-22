{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Harihara.Lastfm (
    module Harihara.Lastfm
  , module H
  ) where

import Control.Lens
import Data.Configurator
import Data.Configurator.Types
import MonadLib
import Network.Lastfm

import Data.Text

import Harihara.Lastfm.Base    as H
import Harihara.Lastfm.Parsers as H
import Harihara.Log

type ConfigFile = Worth FilePath

mkLastfmEnv :: (BaseM m IO) => Config -> m LastfmEnv
mkLastfmEnv c = inBase $ LastfmEnv <$>
  (apiKey <$> require c "api-key") <*>
  (sign <$> Secret <$> require c "secret")

--------------------------------------------------------------------------------

-- | Retrieve a list of artists similar to one given.
getSimilarArtists :: (MonadLastfm m) => Text -> m [ArtistResult]
getSimilarArtists ar = do
  logInfo $ "Lastfm: Getting artists similar to " ++ show ar
  cor <- artist_getCorrection ar
  let ar' = maybe ar (^. artistName) cor
  artist_getSimilar ar'

