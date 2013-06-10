{-# LANGUAGE OverloadedStrings #-}

module Harihara
  ( module Harihara
  , module H
  ) where

import Data.Configurator as Cfg
import Data.Configurator.Types
import Network.Lastfm
import Text.Show.Pretty

import Control.Exception
import Control.Monad (when)
import Data.Set (toList)
import System.Environment
import System.Directory
import System.IO.Error

import Harihara.Log     as H            
import Harihara.Monad   as H            
import Harihara.Options as H            
import Harihara.Utils   as H            

import Harihara.DB      hiding (io)
import Harihara.Lastfm  hiding (io)
import Harihara.Tag     hiding (io)

{-
harihara is the exposed interace to the Harihara Monad.
It uses the intermediary HHBoot Monad so that it can start
logging as soon as the command line arguments are parsed.
-}

-- Running Harihara {{{

-- | Load configuration and options, run a @Harihara@ computation
--   expecting a list of files.
harihara :: [ConfigFile] -> ([FilePath] -> Harihara a) -> IO a
harihara cfs fm = do
  hhOpts <- parseOptions =<< getArgs
  mainCfg <- load cfs
  lfmEnv <- mkLastfmEnv mainCfg hhOpts
  tgEnv  <- mkTagEnv mainCfg hhOpts
  dbOpts <- mkDBOpts mainCfg hhOpts
  let fs = toList $ optsFiles hhOpts
  let hhEnv = buildEnv hhOpts lfmEnv tgEnv dbOpts
  evalHarihara hhEnv $ bracketTagLib $ do
    logDebugData "Harihara Options" $ ppShow hhOpts
    when (dbFresh dbOpts) $ makeFreshDB $ dbPath dbOpts
    fm fs

-- }}}

-- Configuration Aggregation {{{

type ConfigFile = Worth FilePath

mkLastfmEnv :: Config -> HariharaOptions -> IO (Maybe LastfmEnv)
mkLastfmEnv cfg opts = do
  mKey  <- Cfg.lookup cfg "api-key"
  mSign <- Cfg.lookup cfg "secret"
  return $ LastfmEnv           <$>
    (pure $ optsLogLevel opts) <*>
    (apiKey <$> mKey)          <*>
    (sign <$> Secret <$> mSign)

mkTagEnv :: Config -> HariharaOptions -> IO TagEnv
mkTagEnv _ opts = TagEnv   <$> 
  (pure $ optsLogLevel opts)

mkDBOpts :: Config -> HariharaOptions -> IO DBOpts
mkDBOpts cfg opts = DBOpts                      <$>
  lookupDefault (optsDBPath opts) cfg "db-path" <*>
  (pure $ optsDBFresh opts)

-- }}}

-- Helpers {{{

-- | Clean up all remaining TagLib resources, both files and strings.
bracketTagLib :: Harihara a -> Harihara a
bracketTagLib m = do
  logInfo "Running Harihara..."
  a <- m
  logInfo "Harihara finished"
  return a

makeFreshDB :: FilePath -> Harihara ()
makeFreshDB fp = do
  logInfo "Removing existing DB"
  io $ removeIfExists fp
  logInfo "Making fresh DB"
  merr <- sequence <$> withDB fp setupDB
  case merr of
    Nothing  -> logInfo "Done"
    Just err -> do
      logError "Couldn't make a fresh database"
      io $ throwIO $ CantFreshDB $ unlines err

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
  handleExists e
    | isDoesNotExistError e = return ()
    | otherwise = throwIO e

-- }}}
