{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Control.Lens
import Data.Configurator
import Network.Lastfm

import qualified Data.Text as T
import System.Environment

import Config
import Requests
import Parsers.Types

-- | .lastfm_auth must include two keys,
--      api-key :: String
--      secret  :: String
configFiles :: [ConfigFile]
configFiles = [ Required "$(HOME)/.lastfm_auth" ]

getSearchTerm :: IO T.Text
getSearchTerm = T.pack . unwords <$> getArgs

main :: IO ()
main = do
  cfg <- getConfig configFiles
  srch <- getSearchTerm
  ars <- similarArtists srch cfg 
  debug ars print

similarArtists :: MonadDebug r d => T.Text -> LastfmCfg Send -> IO (r [T.Text])
similarArtists art cfg = runRequest $ do
  cor <- artist_getCorrection art cfg
  sim <- artist_getSimilar (cor ^. artistName) cfg
  return $ map (^. artistName) sim

