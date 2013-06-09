{-# LANGUAGE DeriveDataTypeable #-}

module Harihara.Log where

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (when)
import Data.Typeable

-- Exceptions {{{

data LastfmException
  = NoResponse
  | ParseError String
  deriving (Show, Typeable)

instance Exception LastfmException

data TagException
  = FileNotFound FilePath
  deriving (Show,Typeable)

instance Exception TagException

-- }}}

-- Logging {{{

data LogLevel
  = LogSilent
  | LogError
  | LogWarn
  | LogInfo
  | LogDebug
  deriving (Eq, Ord, Show)

class (Functor m, Monad m) => MonadLog m where
  getLogLevel :: m LogLevel
  writeLog    :: String -> m ()
  header      :: m String

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
  hdr <- header
  let fullMsg = unwords
        [ bracketMsgs [ renderLevel lvl , "|" , hdr ]
        , msg
        ]
  when shouldLog $ writeLog fullMsg

bracketMsgs :: [String] -> String
bracketMsgs = ("[ " ++) . (++ " ]") . unwords

renderLevel :: LogLevel -> String
renderLevel ll = case ll of
  LogSilent -> "\"Silent\""
  LogError  -> "Error"
  LogWarn   -> "Warn "
  LogInfo   -> "Info "
  LogDebug  -> "Debug"

-- }}}

