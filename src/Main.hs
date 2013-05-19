{-# LANGUAGE OverloadedStrings #-}

import Data.Configurator

import Control.Monad.IO.Class (liftIO)
import Data.Text

import Harihara

-- | .lastfm_auth must include two keys,
--      api-key :: String
--      secret  :: String
configFiles :: [ConfigFile]
configFiles = [ Required "$(HOME)/.lastfm_auth" ]

main :: IO ()
main = harihara configFiles $ \fs -> do
  as <- tagFiles fs getSongInfo
  liftIO $ print as

