{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Harihara.Lastfm (
    module Harihara.Lastfm
  , module H
  ) where

import Network.Lastfm
import Text.Show.Pretty

import Data.Text

import Harihara.Lastfm.Requests as H
import Harihara.Lastfm.Parsers  as H
import Harihara.Lastfm.Types    as H
import Harihara.Log
import Harihara.Monad
import Harihara.Utils

-- | Retrieve a list of artists similar to one given.
lastfm_similarArtists :: Text -> Harihara (Text,[ArtistSimilar])
lastfm_similarArtists ar = do
  logInfo $ "Lastfm: Getting artists similar to " ++ ppShow ar
  cor <- lastfm_getCorrection_artist ar
  let ar' = maybe (capitalize ar) artistCorrectionName cor
  (ar',) <$> lastfm_getSimilar_artist ar'

