{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Harihara.Monad where

import Control.Lens
import MonadLib

import Harihara.Lastfm
import Harihara.Log
import Harihara.Options
import Harihara.Tag

class (Monad m) => EvalM e m a r | e m a -> r where
  evalM :: e -> m a -> r

-- HHBoot Monad ----------------------------------------------------------------

newtype HHBoot a = HHBoot
  { runHHBoot :: ReaderT HariharaOptions IO a
  } deriving (Functor, Monad)

instance ReaderM HHBoot HariharaOptions where
  ask = HHBoot ask

instance BaseM HHBoot IO where
  inBase = HHBoot . inBase

instance EvalM HariharaOptions HHBoot a (IO a) where
  evalM opts = runReaderT opts . runHHBoot

getBootOpts :: HHBoot HariharaOptions
getBootOpts = ask

fromBootOpts :: (HariharaOptions -> a) ->  HHBoot a
fromBootOpts = asks

instance MonadLog HHBoot where
  getLogLevel = asks _optsLogLevel
  writeLog ll = inBase . putStrLn . (renderLevel ll ++)

-- Harihara Monad --------------------------------------------------------------

newtype Harihara a = Harihara
  { runHarihara :: ReaderT HariharaEnv IO a
  } deriving (Functor, Monad)

instance BaseM Harihara IO where
  inBase = Harihara . inBase

instance ReaderM Harihara HariharaEnv where
  ask = Harihara ask

instance EvalM HariharaEnv Harihara a (IO a) where
  evalM env = runReaderT env . runHarihara

instance MonadLog Harihara where
  getLogLevel = fromHHEnv logLevel
  writeLog ll = inBase . putStrLn . (renderLevel ll ++)

data HariharaEnv = HariharaEnv
  { lastfmEnv :: LastfmEnv
  , logLevel  :: LogLevel
  }

getHHEnv :: Harihara HariharaEnv
getHHEnv = ask

fromHHEnv :: (HariharaEnv -> a) -> Harihara a
fromHHEnv = asks

instance MonadLastfm Harihara where
  getLastfmEnv = fromHHEnv lastfmEnv

instance MonadTag Harihara

-- Running full Harihara from HHBoot Monad
instance EvalM LastfmEnv Harihara a (HHBoot a) where
  evalM lfmEnv m = do
    opts <- getBootOpts
    let hhEnv = buildEnv opts lfmEnv
    inBase $ evalM hhEnv m

buildEnv :: HariharaOptions -> LastfmEnv -> HariharaEnv
buildEnv hhOpts lfmEnv = HariharaEnv
  { lastfmEnv = lfmEnv
  , logLevel  = hhOpts ^. optsLogLevel
  }

renderLevel :: LogLevel -> String
renderLevel ll = case ll of
  LogSilent -> "[ \"Silent\" ] "
  LogError  -> "[ Error ] "
  LogWarn   -> "[ Warn  ] "
  LogInfo   -> "[ Info  ] "
  LogDebug  -> "[ Debug ] "

