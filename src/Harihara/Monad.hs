{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Harihara.Monad where

import MonadLib

import qualified Audio.TagLib.Internal as TL (TagLibException(..))

import Control.Applicative
import Control.Exception
import qualified Data.Text.IO as T

import Harihara.DB     hiding (io)
import Harihara.Lastfm hiding (io, getLastfmEnv)
import Harihara.Tag    hiding (io, getTagEnv)
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

runHarihara :: HariharaEnv -> Harihara a -> IO (a,HariharaEnv)
runHarihara env = runStateT env . unHarihara

evalHarihara :: HariharaEnv -> Harihara a -> IO a
evalHarihara env = evalStateT env . unHarihara

instance BaseM Harihara IO where
  inBase = Harihara . inBase

instance MonadLog Harihara where
  getLogLevel = fromHHEnv logLevel
  writeLog = io . T.putStrLn
  header = return "Main"

catchHarihara :: (Exception e) => Harihara a -> (e -> Harihara a) -> Harihara a
m `catchHarihara` f = do
  env <- getHHEnv
  io $ catch
    (evalHarihara env m)
    (evalHarihara env . f)


-- }}}

-- Harihara Exceptions {{{

-- }}}

-- HariharaEnv {{{

data HariharaEnv = HariharaEnv
  { logLevel     :: LogLevel
  , lastfmEnv    :: Maybe LastfmEnv
  , tagEnv       :: TagEnv
  , databaseOpts :: DBOpts
  }

onLogLevel :: (LogLevel -> LogLevel) -> HariharaEnv -> HariharaEnv
onLogLevel f e = e { logLevel = f $ logLevel e }

onLastfmEnv :: (LastfmEnv -> LastfmEnv) -> HariharaEnv -> HariharaEnv
onLastfmEnv f e = e { lastfmEnv = f <$> lastfmEnv e }

onTagEnv :: (TagEnv -> TagEnv) -> HariharaEnv -> HariharaEnv
onTagEnv f e = e { tagEnv = f $ tagEnv e }

onDatabaseOpts :: (DBOpts -> DBOpts) -> HariharaEnv -> HariharaEnv
onDatabaseOpts f e = e { databaseOpts = f $ databaseOpts e }
 
buildEnv :: HariharaOptions -> Maybe LastfmEnv -> TagEnv -> DBOpts -> HariharaEnv
buildEnv = HariharaEnv . optsLogLevel

-- }}}

-- Monadic Operations {{{

getHHEnv :: Harihara HariharaEnv
getHHEnv = Harihara get

fromHHEnv :: (HariharaEnv -> a) -> Harihara a
fromHHEnv = Harihara . gets

modifyHHEnv :: (HariharaEnv -> HariharaEnv) -> Harihara ()
modifyHHEnv = Harihara . modify

getLastfmEnv :: Harihara (Maybe LastfmEnv)
getLastfmEnv = fromHHEnv lastfmEnv

modifyLastfmEnv :: (LastfmEnv -> LastfmEnv) -> Harihara ()
modifyLastfmEnv = modifyHHEnv . onLastfmEnv

getTagEnv :: Harihara TagEnv
getTagEnv = fromHHEnv tagEnv

modifyTagEnv :: (TagEnv -> TagEnv) -> Harihara ()
modifyTagEnv = modifyHHEnv . onTagEnv

getDatabaseOpts :: Harihara DBOpts
getDatabaseOpts = fromHHEnv databaseOpts

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

tag :: FilePath -> (FileId -> Tag a) -> Harihara a
tag fp f = do
  logInfo "Entering Tag"
  tEnv <- getTagEnv
  io $ runTag tEnv $ withFile fp f

-- | Catch the TagLib exceptions associated with an
--   incompatible or unopenable file. Re-throw any other
--   exception encountered.
skipIfFileBad :: Harihara () -> Harihara ()
skipIfFileBad m = catchHarihara m $ \e -> case e of
  TL.InvalidFile fp -> do
    logWarnData "Invalid TagLib file" $ show fp
    logWarn "Skipping."
  TL.UnableToOpen fp -> do
    logWarnData "TagLib unable to open file" $ show fp
    logWarn "Skipping."
  _ -> io $ throwIO e

-- }}}

-- Lastfm {{{

lastfm :: Lastfm a -> Harihara a
lastfm m = do
  mEnv <- getLastfmEnv
  case mEnv of
    Nothing -> do
      logError "Last.fm API key or Secret are missing from config"
      io $ throwIO MissingLastfmConfig
    Just env -> do
      logInfo "Entering Lastfm"
      io $ runLastfm env m

-- }}}

-- DB {{{

db :: DB a -> Harihara a
db m = do
  fp <- dbPath <$> getDatabaseOpts
  logInfo "Entering DB"
  withDB fp m

withDB :: FilePath -> DB a -> Harihara a
withDB fp m = do
  ll <- getLogLevel
  io $ do
    conn <- openDB fp
    runDB (DBEnv conn ll) $ do
      a <- m
      closeDB
      return a

-- }}}

