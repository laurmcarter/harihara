{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Harihara.Monad where

import MonadLib

import Audio.TagLib hiding (taglib, io, openFile)
import Audio.TagLib.Internal hiding (io)

import Harihara.DB hiding (io)
import Harihara.Lastfm.Types
import Harihara.Log
import Harihara.Options

-- Harihara Monad {{{

newtype Harihara a = Harihara
  { unHarihara :: StateT HariharaEnv IO a
  }

instance Functor Harihara where
  fmap f (Harihara m) = Harihara $ fmap f m

instance Monad Harihara where
  return = Harihara . return
  (Harihara m) >>= f = Harihara $ m >>= unHarihara . f

io :: IO a -> Harihara a
io = Harihara . inBase

runHarihara :: HariharaEnv -> Harihara a -> IO a
runHarihara env = evalStateT env . unHarihara

instance BaseM Harihara IO where
  inBase = Harihara . inBase

instance MonadLog Harihara where
  getLogLevel = fromHHEnv logLevel
  writeLog ll = io . putStrLn . (renderLevel ll ++)

renderLevel :: LogLevel -> String
renderLevel ll = case ll of
  LogSilent -> "[ \"Silent\" ] "
  LogError  -> "[ Error ] "
  LogWarn   -> "[ Warn  ] "
  LogInfo   -> "[ Info  ] "
  LogDebug  -> "[ Debug ] "

-- }}}

-- HariharaEnv {{{

data HariharaEnv = HariharaEnv
  { logLevel    :: LogLevel
  , lastfmEnv   :: LastfmEnv
  , taglibEnv   :: TagLibEnv
  , databaseEnv :: DBEnv
  }

onLogLevel :: (LogLevel -> LogLevel) -> HariharaEnv -> HariharaEnv
onLogLevel f e = e { logLevel = f $ logLevel e }

onLastfmEnv :: (LastfmEnv -> LastfmEnv) -> HariharaEnv -> HariharaEnv
onLastfmEnv f e = e { lastfmEnv = f $ lastfmEnv e }

onTagLibEnv :: (TagLibEnv -> TagLibEnv) -> HariharaEnv -> HariharaEnv
onTagLibEnv f e = e { taglibEnv = f $ taglibEnv e }

onDBEnv :: (DBEnv -> DBEnv) -> HariharaEnv -> HariharaEnv
onDBEnv f e = e { databaseEnv = f $ databaseEnv e }

buildEnv :: HariharaOptions -> LastfmEnv -> TagLibEnv -> DBEnv -> HariharaEnv
buildEnv = HariharaEnv . optsLogLevel

-- }}}

-- Monadic Operations {{{

getHHEnv :: Harihara HariharaEnv
getHHEnv = Harihara get

fromHHEnv :: (HariharaEnv -> a) -> Harihara a
fromHHEnv = Harihara . gets

modifyHHEnv :: (HariharaEnv -> HariharaEnv) -> Harihara ()
modifyHHEnv = Harihara . modify

getTagLibEnv :: Harihara TagLibEnv
getTagLibEnv = fromHHEnv taglibEnv

getLastfmEnv :: Harihara LastfmEnv
getLastfmEnv = fromHHEnv lastfmEnv

getDBEnv :: Harihara DBEnv
getDBEnv = fromHHEnv databaseEnv

modifyTagLibEnv :: (TagLibEnv -> TagLibEnv) -> Harihara ()
modifyTagLibEnv = modifyHHEnv . onTagLibEnv

modifyLastfmEnv :: (LastfmEnv -> LastfmEnv) -> Harihara ()
modifyLastfmEnv = modifyHHEnv . onLastfmEnv

setTagLibEnv :: TagLibEnv -> Harihara ()
setTagLibEnv = modifyTagLibEnv . const

gets :: (StateM m s) => (s -> a) -> m a
gets f = do
  s <- get
  return (f s)

modify :: (StateM m s) => (s -> s) -> m ()
modify f = get >>= set . f

evalStateT :: (Monad m) => s -> StateT s m a -> m a
evalStateT s m = do
 (a,_) <- runStateT s m
 return a

-- }}}

-- Tag {{{

taglib :: TagLib a -> Harihara a
taglib m = do
  env      <- getTagLibEnv
  logInfo "TagLib"
  (a,env') <- io $ runTagLib env m
  logDebug "Updating TagLibEnv"
  setTagLibEnv env'
  return a

-- }}}

-- DB {{{

db :: DB a -> Harihara a
db m = do
  env <- getDBEnv
  logInfo "DB"
  io $ runDB env m

-- }}}

