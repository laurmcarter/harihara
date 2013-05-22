
module Harihara
  ( module Harihara
  , module H
  ) where

import Control.Lens
import Data.Configurator (load)
import Data.Set (toList)
import MonadLib

import Control.Applicative
import System.Environment

import Harihara.Lastfm  as H
import Harihara.Log     as H
import Harihara.Monad   as H
import Harihara.Options as H
import Harihara.Tag     as H
import Harihara.Utils   as H

{-
harihara is the exposed interace to the Harihara Monad.
It uses the intermediary HHBoot Monad so that it can start
logging as soon as the command line arguments are parsed.
-}

-- | Load configuration and options, run a @Harihara@ computation
--   expecting a list of files.
harihara :: [ConfigFile] -> ([FilePath] -> Harihara a) -> IO a
harihara cfs fm = do
  -- parse options to get LogLevel
  opts <- parseOptions <$> getArgs
  evalM opts $ do
    logDebug $ "Boot Options: " ++ show opts
    logInfo "Boot: Loading configuration files"
    mainCfg <- inBase $ load cfs
    logInfo "Boot: Building Lastfm configuration"
    lfmEnv <- inBase $ mkLastfmEnv mainCfg
    let m = fm $ toList $ opts ^. optsFiles
    logInfo "Running Harihara..."
    evalM lfmEnv m

