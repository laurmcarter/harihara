{-# LANGUAGE OverloadedStrings #-}

import Data.Configurator
import MonadLib
import Text.Show.Pretty

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
  forM_ fs $ \f -> skipIfFileBad $ do
    (TagTrack tl art alb _ _ _ _) <- taglib f getTrackInfo
    tr <- lastfm_getInfo_artist_track art tl
    al <- lastfm_getInfo_artist_album art alb
    let t = dbTrack tl art alb f (Just al) (Just tr)
    db $ insertTrack t
    s <- db $ searchByFields "tracks" [("artist",fromString "Art")]
    io $ putStrLn $ ppShow s

