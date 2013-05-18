{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Data.Configurator
import Network.Lastfm

import qualified Data.Text as T
import System.Environment

import Harihara.Lastfm
import Harihara.Tag

-- | .lastfm_auth must include two keys,
--      api-key :: String
--      secret  :: String
configFiles :: [ConfigFile]
configFiles = [ Required "$(HOME)/.lastfm_auth" ]

getSearchTerm :: IO T.Text
getSearchTerm = T.pack . unwords <$> getArgs

main :: IO ()
main = do
  cfg <- load configFiles
  lfmEnv <- mkLastfmEnv cfg
  return ()

