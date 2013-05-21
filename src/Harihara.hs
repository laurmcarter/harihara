
module Harihara
  ( module Harihara
  , module H
  ) where

import Control.Lens
import Data.Configurator (load)
import Data.Set (toList)

import Control.Monad.IO.Class

import Harihara.Lastfm  as H
import Harihara.Log     as H
import Harihara.Monad   as H
import Harihara.Options as H
import Harihara.Tag     as H
import Harihara.Utils   as H

harihara :: [ConfigFile] -> HariharaOptions -> ([FilePath] -> Harihara a) -> IO a
harihara cfs opts fm = flip evalHHBoot opts $ do
  logDebug $ "Boot Options: " ++ show opts
  logInfo "Boot: Loading configuration files"
  mainCfg <- liftIO $ load cfs
  logInfo "Boot: Building Lastfm configuration"
  lfmEnv <- liftIO $ mkLastfmEnv mainCfg
  let m = fm $ toList $ opts ^. optsFiles
  logInfo "Running Harihara..."
  bootLastfm m lfmEnv

