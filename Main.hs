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

configFiles :: [ConfigFile]
configFiles = [ Required "$(HOME)/.lastfm_auth" ]

getSearchTerm :: IO T.Text
getSearchTerm = T.pack . unwords <$> getArgs

main :: IO ()
main = do
  cfg <- getConfig configFiles
  srch <- getSearchTerm
  putStrLn $ "Looking for corrections to " ++ show srch
  mArt <- artist_getCorrection srch cfg
  debug mArt $ withArtist cfg

withArtist :: LastfmCfg Send -> ArtistResult -> IO ()
withArtist cfg a = do
  let nm = a ^. artistName
  putStrLn $ "Found artist " ++ show nm
  inf <- artist_getInfo nm cfg
  debug inf print

