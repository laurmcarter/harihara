{-# LANGUAGE OverloadedStrings #-}

import Audio.TagLib
import Data.Configurator
import MonadLib

import Control.Applicative    ( (<$>) )
import Control.Lens           ( view )
import Data.Maybe             ( listToMaybe )
import Data.Text as T         ( unlines )
import qualified Data.Text.IO as T ( putStrLn )

import Harihara

-- | .lastfm_auth must include keys,
--      api-key :: String
--      secret  :: String
configFiles :: [ConfigFile]
configFiles =
  [ Optional "$(HOME)/.lastfm_auth"
  , Required "$(HOME)/.harihara"
  ]

main :: IO ()
main = harihara configFiles $ \fs -> do
  as <- tagWithFiles fs getSongInfo
  let mn = songArtist <$> (listToMaybe =<< as)
  whenJust mn $ \artNm -> do
    (artNm',sims) <- getSimilarArtists artNm
    let ns = map (view artistName) sims
    inBase $ do
      putStrLn $ "Similar artists to " ++ show artNm' ++ ":\n"
      T.putStrLn $ T.unlines $ take 10 ns

