{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Harihara.Lastfm (
    module Harihara.Lastfm
  , module H
  ) where

import Data.Configurator
import Data.Configurator.Types
import Network.Lastfm

import Data.Text

import Harihara.Lastfm.Base    as H
import Harihara.Lastfm.Parsers as H
import Harihara.Log
import Harihara.Utils

type ConfigFile = Worth FilePath

mkLastfmEnv :: Config -> IO LastfmEnv
mkLastfmEnv c = LastfmEnv          <$>
  (apiKey <$> require c "api-key") <*>
  (sign <$> Secret <$> require c "secret")

--------------------------------------------------------------------------------

-- | Retrieve a list of artists similar to one given.
getSimilarArtists :: (MonadLastfm m) => Text -> m (Text,[ArtistResult])
getSimilarArtists ar = do
  logInfo $ "Lastfm: Getting artists similar to " ++ show ar
  cor <- artist_getCorrection ar
  let ar' = maybe (capitalize ar) artistName cor
  (ar',) <$> artist_getSimilar ar'

