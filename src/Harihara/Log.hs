{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Harihara.Log where

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (when)
import Data.List as L
import Data.Text as T
import Data.Typeable

-- Exceptions {{{

data LastfmException
  = NoResponse
  | JSONParseError String
  deriving (Typeable)

instance Show LastfmException where
  show e = case e of
    NoResponse        ->
      "No response from last.fm"
    JSONParseError js ->
      "Failed to parse last.fm's JSON response:\n" ++ show js

instance Exception LastfmException

data TagException
  = FileNotFound FilePath
  deriving (Show,Typeable)

instance Exception TagException

-- }}}

-- Log functions {{{

logError :: (MonadLog m) => Text -> m ()
logError  = filterLog LogError

logWarn  :: (MonadLog m) => Text -> m ()
logWarn   = filterLog LogWarn

logInfo  :: (MonadLog m) => Text -> m ()
logInfo   = filterLog LogInfo

logDebug :: (MonadLog m) => Text -> m ()
logDebug  = filterLog LogDebug

logErrorData :: (MonadLog m) => Text -> String -> m ()
logErrorData msg dat = logError $ T.unlines (msg:dat')
  where
  dat' = indentData 2 $ T.pack dat

logWarnData :: (MonadLog m) => Text -> String -> m ()
logWarnData msg dat = logWarn $ T.unlines (msg:dat')
  where
  dat' = indentData 2 $ T.pack dat

logInfoData :: (MonadLog m) => Text -> String -> m ()
logInfoData msg dat = logInfo $ T.unlines (msg:dat')
  where
  dat' = indentData 2 $ T.pack dat

logDebugData :: (MonadLog m) => Text -> String -> m ()
logDebugData msg dat = logDebug $ T.unlines (msg:dat')
  where
  dat' = indentData 2 $ T.pack dat

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
  writeLog    :: Text -> m ()
  header      :: m Text

indentData :: Int -> Text -> [Text]
indentData n = L.map (T.replicate n " " `T.append`) . T.lines

filterLog :: (MonadLog m) => LogLevel -> Text -> m ()
filterLog lvl msg = do
  shouldLog <- (lvl <=) <$> getLogLevel
  hdr <- header
  let fullMsg = T.concat
        [ justifyLeft 20 ' ' (bracketMsgs [ renderLevel lvl , "|" , hdr ])
        ,msg
        ]
  when shouldLog $ writeLog fullMsg

bracketMsgs :: [Text] -> Text
bracketMsgs = ("[ " `append`) . (`append` " ]") . T.unwords

renderLevel :: LogLevel -> Text
renderLevel ll = case ll of
  LogSilent -> "\"Silent\""
  LogError  -> "Error"
  LogWarn   -> "Warn"
  LogInfo   -> "Info"
  LogDebug  -> "Debug"

-- }}}

