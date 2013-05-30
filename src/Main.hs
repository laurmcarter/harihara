{-# LANGUAGE OverloadedStrings #-}

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
  forM_ fs $ \f -> skipIfFileBad $ do
    inf@(SongInfo tl art alb _ _ _ _) <- taglib f getSongInfo
    tr <- lastfm_getInfo_artist_track art "Moanin"
    let row = SongRow tl art alb (trackMBId tr) f
    db $ insertSong row
    db $ searchByFields [("artist",fromString "Art")]
    return ()

