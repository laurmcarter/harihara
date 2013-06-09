{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Harihara.Lastfm.Types where

import MonadLib
import Harihara.Log
import Network.Lastfm

import Control.Applicative ()

-- Lastfm Monad {{{

newtype Lastfm a = Lastfm { unLastfm :: ReaderT LastfmEnv IO a }
  deriving (Functor,Monad,Applicative)

instance MonadLog Lastfm where
  getLogLevel = getsLastfmEnv lfmLogLevel
  writeLog = io . putStrLn
  header = return "Lastfm"

runLastfm :: LastfmEnv -> Lastfm a -> IO a
runLastfm e = runReaderT e . unLastfm

localLastfm :: LastfmEnv -> Lastfm a -> Lastfm a
localLastfm env = Lastfm . local env . unLastfm

getsLastfmEnv :: (LastfmEnv -> a) -> Lastfm a
getsLastfmEnv = Lastfm . asks

getLastfmEnv :: Lastfm LastfmEnv
getLastfmEnv = Lastfm ask

changeLogLevel :: LogLevel -> Lastfm a -> Lastfm a
changeLogLevel ll m = do
  env <- getLastfmEnv
  let env' = setLfmLogLevel ll env
  localLastfm env' m

io :: IO a -> Lastfm a
io = Lastfm . inBase

-- }}}

-- LastfmEnv {{{

type LfmRequest = Request JSON Send
type LfmRequestAuth = Request JSON Sign

type KeyedRequest = LfmRequest (APIKey -> Ready)

data LastfmEnv = LastfmEnv
  { lfmLogLevel :: LogLevel
  , getApiKey   :: LfmRequest APIKey
  , signRequest :: LfmRequestAuth Ready -> LfmRequest Ready
  }

setLfmLogLevel :: LogLevel -> LastfmEnv -> LastfmEnv
setLfmLogLevel ll e = e { lfmLogLevel = ll }

-- }}}

