
module Harihara
  ( module Harihara
  , module H
  ) where

import Data.Configurator (load)
import Data.Set (toList)

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
  hhOpts <- parseOptions <$> getArgs
  mainCfg <- load cfs
  lfmEnv <- mkLastfmEnv mainCfg
  let m = fm $ toList $ optsFiles $ hhOpts
  let hhEnv = buildEnv hhOpts lfmEnv
  runHarihara hhEnv m

