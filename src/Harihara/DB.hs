{-# LANGUAGE OverloadedStrings #-}

module Harihara.DB where

import Database.SQLite
import Database.SQLite.Types
import MonadLib

import Control.Applicative

-- DB Monad {{{

newtype DB a = DB { unDB :: ReaderT DBEnv IO a }

instance Functor DB where
  fmap f (DB m) = DB $ fmap f m

instance Monad DB where
  return = DB . return
  (DB m) >>= f = DB $ m >>= unDB . f

instance Applicative DB where
  pure = return
  (<*>) = ap

runDB :: DBEnv -> DB a -> IO a
runDB env m = runReaderT env $ unDB m

io :: IO a -> DB a
io = DB . inBase

-- }}}

-- DBEnv {{{

data DBEnv = DBEnv
  { dbPath :: FilePath
  , dbConn :: SQLiteHandle
  }

getEnv :: DB DBEnv
getEnv = DB ask

fromEnv :: (DBEnv -> a) -> DB a
fromEnv = DB . asks

getConn :: DB SQLiteHandle
getConn = fromEnv dbConn

getPath :: DB FilePath
getPath = fromEnv dbPath

-- }}}

openDB :: FilePath -> IO SQLiteHandle
openDB fp = openConnection fp

closeDB :: DB ()
closeDB = do
  conn <- getConn
  io $ closeConnection conn

