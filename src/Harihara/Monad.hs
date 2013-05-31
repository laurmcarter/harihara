{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Harihara.Monad where

import MonadLib

import qualified Audio.TagLib as TL
import Audio.TagLib.Internal hiding (io)
import Text.Show.Pretty (ppShow)

import Control.Applicative
import Control.Exception

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

runHarihara :: HariharaEnv -> Harihara a -> IO (a,HariharaEnv)
runHarihara env = runStateT env . unHarihara

evalHarihara :: HariharaEnv -> Harihara a -> IO a
evalHarihara env = evalStateT env . unHarihara

instance BaseM Harihara IO where
  inBase = Harihara . inBase

instance MonadLog Harihara where
  getLogLevel = fromHHEnv logLevel
  writeLog ll = io . putStrLn . (renderLevel ll ++)

catchHarihara :: (Exception e) => Harihara a -> (e -> Harihara a) -> Harihara a
m `catchHarihara` f = do
  env <- getHHEnv
  io $ catch
    (evalHarihara env m)
    (evalHarihara env . f)


-- }}}

-- HariharaEnv {{{

data HariharaEnv = HariharaEnv
  { logLevel     :: LogLevel
  , lastfmEnv    :: LastfmEnv
  , databaseOpts :: DBOpts
  }

onLogLevel :: (LogLevel -> LogLevel) -> HariharaEnv -> HariharaEnv
onLogLevel f e = e { logLevel = f $ logLevel e }

onLastfmEnv :: (LastfmEnv -> LastfmEnv) -> HariharaEnv -> HariharaEnv
onLastfmEnv f e = e { lastfmEnv = f $ lastfmEnv e }

onDatabaseOpts :: (DBOpts -> DBOpts) -> HariharaEnv -> HariharaEnv
onDatabaseOpts f e = e { databaseOpts = f $ databaseOpts e }

buildEnv :: HariharaOptions -> LastfmEnv -> DBOpts -> HariharaEnv
buildEnv = HariharaEnv . optsLogLevel

-- }}}

-- Monadic Operations {{{

getHHEnv :: Harihara HariharaEnv
getHHEnv = Harihara get

fromHHEnv :: (HariharaEnv -> a) -> Harihara a
fromHHEnv = Harihara . gets

modifyHHEnv :: (HariharaEnv -> HariharaEnv) -> Harihara ()
modifyHHEnv = Harihara . modify

getLastfmEnv :: Harihara LastfmEnv
getLastfmEnv = fromHHEnv lastfmEnv

getDatabaseOpts :: Harihara DBOpts
getDatabaseOpts = fromHHEnv databaseOpts

modifyLastfmEnv :: (LastfmEnv -> LastfmEnv) -> Harihara ()
modifyLastfmEnv = modifyHHEnv . onLastfmEnv

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

taglib :: FilePath -> (FileId -> TL.TagLib a) -> Harihara a
taglib fp f = do
  logInfo "TagLib"
  io $ TL.taglib (f =<< TL.openFile fp)

-- | Catch the TagLib exceptions associated with an
--   incompatible or unopenable file. Re-throw any other
--   exception encountered.
skipIfFileBad :: Harihara () -> Harihara ()
skipIfFileBad m = catchHarihara m $ \e -> case e of
  InvalidFile fp -> do
    logWarn $ "Invalid TagLib file: " ++ ppShow fp
    logWarn "Skipping."
  UnableToOpen fp -> do
    logWarn $ "TagLib unable to open file: " ++ ppShow fp
    logWarn "Skipping."
  _ -> io $ throw e

-- }}}

-- DB {{{

db :: DB a -> Harihara a
db m = do
  fp <- dbPath <$> getDatabaseOpts
  logInfo "DB"
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

