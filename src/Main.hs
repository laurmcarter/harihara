{-# LANGUAGE OverloadedStrings #-}

import Data.Configurator
import MonadLib
import System.Directory
import System.FilePath
import Text.Show.Pretty

import Harihara
import Harihara.DB     hiding (io)
import Harihara.Lastfm hiding (io)
import Harihara.Tag    hiding (io)

-- | The sum configuration must contain the following fields:
--      music-dirs :: [String]
--   It must contain these fields if the program is to use
--   a lastfm operation:
--      api-key    :: String
--      secret     :: String
--   These may be placed in either .harihara or .lastfm_auth,
--   at the user's discretion.
configFiles :: [ConfigFile]
configFiles =
  [ Required "$(HOME)/.harihara"
  , Optional "$(HOME)/.lastfm_auth"
  ]

main :: IO ()
main = harihara configFiles $ \fs -> do
  -- Skip the bad files, eg. non-existent, non-music, etc.
  forM_ fs $ \f -> skipIfFileBad $ do

    -- Resolve the absolute path of the file
    pwd <- io getCurrentDirectory
    let fp = pwd </> f

    -- Get the tags off the file
    tl <- tag fp getTitle
    art <- tag fp getArtist
    alb <- tag fp getAlbum

    -- Get Last.fm's info on the track and album
    trkInf <- lastfm $ getInfo_artist_track art tl
    albInf <- lastfm $ getInfo_artist_album art alb

    -- Compile the info needed for a database entry
    let t = dbTrack tl art alb fp (Just albInf) (Just trkInf)

    -- Insert track into database, then search for it.
    db $ insertTrack t
    s <- db $ searchByFields "tracks" [("artist",fromString "Art")]

    -- Print the results
    io $ putStrLn $ ppShow $ map dbTrackTitle s

