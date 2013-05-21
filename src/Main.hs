{-# LANGUAGE OverloadedStrings #-}

import Data.Configurator
import MonadLib

import Control.Applicative    ( (<$>) )
import Control.Lens           ( view )
import Data.Maybe             ( listToMaybe )
import Data.Text as T         ( unlines, append )
import Data.Text.IO as T      ( putStrLn )
import System.Environment     ( getArgs )

import Harihara

-- | .lastfm_auth must include keys,
--      api-key :: String
--      secret  :: String
configFiles :: [ConfigFile]
configFiles = [ Required "$(HOME)/.lastfm_auth" ]

main :: IO ()
main = do
  opts <- parseOptions <$> getArgs
  harihara configFiles opts $ \fs -> do
    as <- tagWithFiles fs getSongInfo
    logInfo $ show as
    let mn = songArtist <$> (listToMaybe =<< as)
    whenJust mn $ \artNm -> do
      sims <- getSimilarArtists artNm
      let ns = map (view artistName) sims
      inBase $ do
        T.putStrLn $ "Similar artists to " `append` artNm
        T.putStrLn $ T.unlines ns
        

