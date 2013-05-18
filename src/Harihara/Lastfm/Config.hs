{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Harihara.Lastfm.Config where

import Data.Configurator
import Data.Configurator.Types
import Network.Lastfm

import Harihara.Lastfm.Base

type ConfigFile = Worth FilePath

mkLastfmEnv :: Config -> IO LastfmEnv
mkLastfmEnv c = LastfmEnv          <$>
  (apiKey <$> require c "api-key") <*>
  (sign <$> Secret <$> require c "secret")

loadLastfmEnv :: [ConfigFile] -> IO LastfmEnv
loadLastfmEnv fs = load fs >>= mkLastfmEnv

