{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import Control.Lens
import Data.Configurator
import Network.Lastfm

import qualified Data.Text as T
import System.Environment

import Harihara.Lastfm.Config
import Harihara.Lastfm.Requests
import Harihara.Lastfm.Parsers.Types

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
  sim <- runRequest $ do
    cor <- artist_getCorrection srch cfg
    sim <- artist_getSimilar (cor ^. artistName) cfg
    return $ map (^. artistName) sim
  debug print sim

