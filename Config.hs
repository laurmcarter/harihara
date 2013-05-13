{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Config where

import Data.Configurator
import Data.Configurator.Types
import Network.Lastfm

type ConfigFile = Worth FilePath

data LastfmCfg a = LCfg
  { getApiKey   :: Request JSON a APIKey
  , signRequest :: Request JSON Sign Ready -> Request JSON Send Ready
  }

mkLastfmCfg :: Config -> IO (LastfmCfg a)
mkLastfmCfg c = LCfg                       <$>
  (apiKey <$> require c "api-key")         <*>
  (sign <$> Secret <$> require c "secret")

getConfig :: [ConfigFile] -> IO (LastfmCfg auth)
getConfig fs = load fs >>= mkLastfmCfg

