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

import Harihara.Lastfm.Requests as H
import Harihara.Lastfm.Parsers  as H
import Harihara.Lastfm.Types    as H
import Harihara.Log
import Harihara.Monad
import Harihara.Utils

type ConfigFile = Worth FilePath

mkLastfmEnv :: Config -> IO LastfmEnv
mkLastfmEnv c = LastfmEnv          <$>
  (apiKey <$> require c "api-key") <*>
  (sign <$> Secret <$> require c "secret")

--------------------------------------------------------------------------------

-- | Retrieve a list of artists similar to one given.
lastfm_similarArtists :: Text -> Harihara (Text,[ArtistResult])
lastfm_similarArtists ar = do
  logInfo $ "Lastfm: Getting artists similar to " ++ show ar
  cor <- lastfm_getCorrection_artist ar
  let ar' = maybe (capitalize ar) artistName cor
  (ar',) <$> lastfm_getSimilar_artist ar'

