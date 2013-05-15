{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Harihara.Lastfm.Config where

import Data.Configurator
import Data.Configurator.Types
import Network.Lastfm

import Harihara.Lastfm.Base

type ConfigFile = Worth FilePath

mkLastfmCfg :: Config -> IO (LastfmCfg a)
mkLastfmCfg c = LCfg                       <$>
  (apiKey <$> require c "api-key")         <*>
  (sign <$> Secret <$> require c "secret")

getConfig :: [ConfigFile] -> IO (LastfmCfg auth)
getConfig fs = load fs >>= mkLastfmCfg

