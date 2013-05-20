
module Harihara.Monad where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Harihara.Lastfm
import Harihara.Log
import Harihara.Options
import Harihara.Tag

newtype HHBoot a = HHBoot
  { runHHBoot :: ReaderT HariharaOptions IO a
  }

instance Functor HHBoot where
  fmap f (HHBoot m) = HHBoot $ fmap f m

instance Monad HHBoot where
  return = HHBoot . return
  (HHBoot m) >>= f = HHBoot (m >>= runHHBoot . f)

instance Applicative HHBoot where
  pure = return
  (<*>) = ap

getOpts :: HHBoot HariharaOptions
getOpts = HHBoot ask

evalHHBoot :: HHBoot a -> HariharaOptions -> IO a
evalHHBoot m opts = flip runReaderT opts $ runHHBoot m

instance MonadIO HHBoot where
  liftIO = HHBoot . ReaderT . const

instance MonadLog HHBoot where
  getLogLevel = _optsLogLevel <$> getOpts
  writeLog ll = liftIO . putStrLn . (renderLevel ll ++)
  

newtype Harihara a = Harihara
  { runHarihara :: ReaderT HariharaEnv IO a }

data HariharaEnv = HariharaEnv
  { lastfmEnv :: LastfmEnv
  , logLevel  :: LogLevel
  }

evalHarihara :: Harihara a -> HariharaEnv -> IO a
evalHarihara m env = flip runReaderT env $ runHarihara m

getEnv :: Harihara HariharaEnv
getEnv = Harihara ask

fromEnv :: (HariharaEnv -> a) -> Harihara a
fromEnv f = f <$> getEnv

bootLastfm :: Harihara a -> LastfmEnv -> HHBoot a
bootLastfm m lfmEnv = do
  opts <- getOpts
  let hhEnv = buildEnv opts lfmEnv
  liftIO $ evalHarihara m hhEnv

buildEnv :: HariharaOptions -> LastfmEnv -> HariharaEnv
buildEnv hhOpts lfmEnv = HariharaEnv
  { lastfmEnv = lfmEnv
  , logLevel  = hhOpts ^. optsLogLevel
  }

instance Functor Harihara where
  fmap f (Harihara m) = Harihara $ fmap f m

instance Applicative Harihara where
  pure  = return
  (<*>) = ap

instance Monad Harihara where
  return = Harihara . return
  (Harihara m) >>= f = Harihara (m >>= runHarihara . f)

instance MonadIO Harihara where
  liftIO = Harihara . ReaderT . const

instance MonadLog Harihara where
  getLogLevel = fromEnv logLevel
  writeLog ll = liftIO . putStrLn . (renderLevel ll ++)

renderLevel :: LogLevel -> String
renderLevel ll = case ll of
  LogError -> "[ Error ] "
  LogWarn  -> "[ Warn  ] "
  LogInfo  -> "[ Info  ] "
  LogDebug -> "[ Debug ] "

instance MonadLastfm Harihara where
  getLastfmEnv = fromEnv lastfmEnv

instance MonadTag Harihara

