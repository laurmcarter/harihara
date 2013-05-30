{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Harihara
  ( module Harihara
  , module H
  ) where

import Audio.TagLib.Internal hiding (io, removeFile)

import Data.Configurator
import Data.Configurator.Types
import Network.Lastfm

import Control.Exception
import Control.Monad (when)
import Data.Set (toList)
import Data.Typeable (Typeable())
import System.Environment
import System.Directory
import System.IO.Error

import Harihara.DB      as H hiding (io)
import Harihara.Lastfm  as H
import Harihara.Log     as H            
import Harihara.Monad   as H            
import Harihara.Options as H            
import Harihara.Tag     as H            
import Harihara.Utils   as H            

data HariharaException
  = Usage String
  | CantFreshDB String
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
  dbOpts <- mkDBOpts mainCfg hhOpts
  let tlEnv = initialEnv
  let m = fm $ toList $ optsFiles $ hhOpts
  let hhEnv = buildEnv hhOpts lfmEnv tlEnv dbOpts
  evalHarihara hhEnv $ bracketTagLib $ do
    when (dbFresh dbOpts) $ makeFreshDB $ dbPath dbOpts
    m

-- | Clean up all remaining TagLib resources, both files and strings.
bracketTagLib :: Harihara a -> Harihara a
bracketTagLib m = do
  logInfo "Running Harihara..."
  a <- m
  --fs <- taglib openFilePtrs
  --let n = length fs
  --logInfo $ "Closing " ++ show n ++ " TagLib file" ++ (if n /= 1 then "s" else "")
  --io (mapM_ cleanupFile fs >> freeTagLibStrings)
  logInfo "Harihara finished"
  return a

-- Configuration Aggregation {{{

type ConfigFile = Worth FilePath

mkLastfmEnv :: Config -> IO LastfmEnv
mkLastfmEnv c = LastfmEnv          <$>
  (apiKey <$> require c "api-key") <*>
  (sign <$> Secret <$> require c "secret")

mkDBOpts :: Config -> HariharaOptions -> IO DBOpts
mkDBOpts cfg opts = DBOpts                      <$>
  lookupDefault (optsDBPath opts) cfg "db-path" <*>
  (pure $ optsDBFresh opts)

-- }}}

makeFreshDB :: FilePath -> Harihara ()
makeFreshDB fp = do
  logInfo "Removing existing DB"
  io $ removeIfExists fp
  logInfo "Making fresh DB"
  merr <- withDB fp setupTable
  case merr of
    Nothing  -> logInfo "Done"
    Just err -> do
      logError "Couldn't make a fresh database"
      io $ throw $ CantFreshDB err
      

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
  handleExists e
    | isDoesNotExistError e = return ()
    | otherwise = throwIO e
