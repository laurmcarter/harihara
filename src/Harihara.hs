{-# LANGUAGE DeriveDataTypeable #-}

module Harihara
  ( module Harihara
  , module H
  ) where

import Audio.TagLib.Internal hiding (io)

import Data.Configurator (load)
import Data.Set (toList)
import Data.Typeable (Typeable())

import Control.Applicative
import Control.Exception
import System.Environment

import Harihara.Lastfm  as H
import Harihara.Log     as H
import Harihara.Monad   as H
import Harihara.Options as H
import Harihara.Tag     as H
import Harihara.Utils   as H

data HariharaException
  = Usage String
  deriving (Show,Typeable)

instance Exception HariharaException

{-
harihara is the exposed interace to the Harihara Monad.
It uses the intermediary HHBoot Monad so that it can start
logging as soon as the command line arguments are parsed.
-}

-- | Load configuration and options, run a @Harihara@ computation
--   expecting a list of files.
harihara :: [ConfigFile] -> ([FilePath] -> Harihara a) -> IO a
harihara cfs fm = do
  mOpts <- parseOptions <$> getArgs
  hhOpts <- either (throw . Usage) return mOpts
  mainCfg <- load cfs
  lfmEnv <- mkLastfmEnv mainCfg
  let m = fm $ toList $ optsFiles $ hhOpts
  let hhEnv = buildEnv hhOpts lfmEnv initialEnv
  runHarihara hhEnv $ bracketTagLib m

-- | Clean up all remaining TagLib resources, both files and strings.
bracketTagLib :: Harihara a -> Harihara a
bracketTagLib m = do
  logInfo "Running Harihara..."
  a <- m
  fs <- taglib openFilePtrs
  let n = length fs
  logInfo $ "Closing " ++ show n ++ " TagLib file" ++ (if n /= 1 then "s" else "")
  io (mapM_ cleanupFile fs >> freeTagLibStrings)
  logInfo "Done!"
  return a

