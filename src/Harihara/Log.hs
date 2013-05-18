{-# LANGUAGE DeriveDataTypeable #-}

module Harihara.Log (
    LogLevel (..)
  , LastfmException (..)
  , TagException (..)
  , MonadLog (..)
  , logError
  , logWarn
  , logInfo
  , logDebug
  ) where

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (when)
import Data.Typeable

-- Exceptions ------------------------------------------------------------------

data LastfmException
  = NoResponse
  | ParseError String
  deriving (Show, Typeable)

instance Exception LastfmException

data TagException
  = FileNotFound FilePath
  deriving (Show,Typeable)

instance Exception TagException

-- Logging ---------------------------------------------------------------------

data LogLevel
  = LogError
  | LogWarn
  | LogInfo
  | LogDebug
  deriving (Eq, Ord)

class (Functor m, Monad m) => MonadLog m where
  getLogLevel :: m LogLevel
  writeLog    :: String -> m ()

logError :: (MonadLog m) => String -> m ()
logError  = filterLog LogError

logWarn  :: (MonadLog m) => String -> m ()
logWarn   = filterLog LogWarn

logInfo  :: (MonadLog m) => String -> m ()
logInfo   = filterLog LogInfo

logDebug :: (MonadLog m) => String -> m ()
logDebug  = filterLog LogDebug

filterLog :: (MonadLog m) => LogLevel -> String -> m ()
filterLog lvl msg = do
  shouldLog <- (lvl <=) <$> getLogLevel
  when shouldLog $ writeLog msg

