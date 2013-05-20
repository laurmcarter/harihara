{-# LANGUAGE OverloadedStrings #-}

import Data.Configurator

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Text
import System.Environment

import Harihara

-- | .lastfm_auth must include two keys,
--      api-key :: String
--      secret  :: String
configFiles :: [ConfigFile]
configFiles = [ Required "$(HOME)/.lastfm_auth" ]

main :: IO ()
main = do
  opts <- parseOptions <$> getArgs
  harihara configFiles opts $ \fs -> do
    as <- tagFiles fs getSongInfo
    logDebug $ show as

