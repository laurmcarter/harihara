{-# LANGUAGE OverloadedStrings #-}

import Audio.TagLib hiding (io, taglib)
import Data.Configurator
import MonadLib

import Harihara

-- | The sum configuration must contain the following fields:
--      api-key    :: String
--      secret     :: String
--      music-dirs :: [String]
--   These may be placed in either .harihara or .lastfm_auth,
--   at the user's discretion.
configFiles :: [ConfigFile]
configFiles =
  [ Required "$(HOME)/.harihara"
  , Optional "$(HOME)/.lastfm_auth"
  ]

main :: IO ()
main = harihara configFiles $ \fs -> do
  fids <- taglib $ mapM openFile fs 
  forM_ fids $ \fid -> do
    art <- taglib $ getArtist fid
    io $ putStrLn $ "Artist is: " ++ show art
    (art',sims) <- lastfm_similarArtists art
    when (art /= art') $ taglib $ setArtist fid art'
    io $ do
      putStrLn "10 Similar Artists:"
      mapM_ (print . artistName) $ take 10 sims

