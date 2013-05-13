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

similarArtists :: DebugLevel d => T.Text -> LastfmCfg Send -> IO (d [T.Text])
similarArtists art cfg = do
  cor <- artist_getCorrection art cfg
  finalAnd cor $ \a -> do
    sim <- artist_getSimilar (a ^. artistName) cfg
    return $ map (^. artistName) <$> sim

withArtist :: LastfmCfg Send -> ArtistResult -> IO ()
withArtist cfg a = do
  let nm = a ^. artistName
  putStrLn $ "Found artist " ++ show nm
  sim <- artist_getSimilar nm cfg
  debug sim (print . map (^. artistName))

