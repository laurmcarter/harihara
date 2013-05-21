
module Harihara
  ( module Harihara
  , module H
  ) where

import Control.Lens
import Data.Configurator (load)
import Data.Set (toList)
import MonadLib

import Harihara.Lastfm  as H
import Harihara.Log     as H
import Harihara.Monad   as H
import Harihara.Options as H
import Harihara.Tag     as H
import Harihara.Utils   as H

harihara :: [ConfigFile] -> HariharaOptions -> ([FilePath] -> Harihara a) -> IO a
harihara cfs opts fm = eval opts $ do
  logDebug $ "Boot Options: " ++ show opts
  logInfo "Boot: Loading configuration files"
  mainCfg <- inBase $ load cfs
  logInfo "Boot: Building Lastfm configuration"
  lfmEnv <- inBase $ mkLastfmEnv mainCfg
  let m = fm $ toList $ opts ^. optsFiles
  logInfo "Running Harihara..."
  eval lfmEnv m

