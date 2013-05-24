{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Harihara.Monad where

import MonadLib

import Harihara.Lastfm
import Harihara.Log
import Harihara.Options
import Harihara.Tag

-- Harihara Monad {{{

newtype Harihara a = Harihara
  { unHarihara :: ReaderT HariharaEnv IO a
  }

instance Functor Harihara where
  fmap f (Harihara m) = Harihara $ fmap f m

instance Monad Harihara where
  return = Harihara . return
  (Harihara m) >>= f = Harihara $ m >>= unHarihara . f

io :: IO a -> Harihara a
io = Harihara . inBase

runHarihara :: HariharaEnv -> Harihara a -> IO a
runHarihara env = runReaderT env . unHarihara

instance BaseM Harihara IO where
  inBase = Harihara . inBase

instance MonadLog Harihara where
  getLogLevel = fromHHEnv logLevel
  writeLog ll = io . putStrLn . (renderLevel ll ++)

instance MonadLastfm Harihara where
  getLastfmEnv = fromHHEnv lastfmEnv

instance MonadTag Harihara

data HariharaEnv = HariharaEnv
  { lastfmEnv :: LastfmEnv
  , logLevel  :: LogLevel
  }

getHHEnv :: Harihara HariharaEnv
getHHEnv = Harihara ask

fromHHEnv :: (HariharaEnv -> a) -> Harihara a
fromHHEnv = Harihara . asks

buildEnv :: HariharaOptions -> LastfmEnv -> HariharaEnv
buildEnv hhOpts lfmEnv = HariharaEnv
  { lastfmEnv = lfmEnv
  , logLevel  = optsLogLevel hhOpts
  }

renderLevel :: LogLevel -> String
renderLevel ll = case ll of
  LogSilent -> "[ \"Silent\" ] "
  LogError  -> "[ Error ] "
  LogWarn   -> "[ Warn  ] "
  LogInfo   -> "[ Info  ] "
  LogDebug  -> "[ Debug ] "

-- }}}

