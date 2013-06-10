{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Harihara.Options 
  ( parseOptions
  , HariharaOptions (..)
  , HariharaException (..)
  ) where

import Control.Exception
import Data.Foldable (foldrM)
import qualified Data.Set as S
import Data.Typeable (Typeable())

import System.Console.GetOpt
import System.Directory
import System.FilePath.Posix
import System.Exit

import Harihara.Log

-- Harihara Exceptions {{{

data HariharaException
  = CantFreshDB String
  | FileDoesNotExist String
  | InvalidPath String
  | NoFlagParse String String
  | MissingLastfmConfig
  deriving (Typeable)

instance Show HariharaException where
  show e = case e of
    CantFreshDB fp       ->
      "Can't make a fresh database with path " ++ show fp
    FileDoesNotExist fp  -> 
      "File does not exist: " ++ show fp
    InvalidPath fp       -> 
      "Not a valid filepath: " ++ show fp
    NoFlagParse flag arg -> 
      "Couldn't parse arg for flag " ++ show flag ++ ": " ++ show arg
    MissingLastfmConfig  -> 
      "Can't use Lastfm function, config didn't define API key or Secret"

instance Exception HariharaException

-- }}}

-- HariharaOptions {{{

data HariharaOptions = HariharaOptions
  { optsLogLevel :: LogLevel
  , optsFiles    :: S.Set FilePath
  , optsDBPath   :: FilePath
  , optsDBFresh  :: Bool
  } deriving (Show)

defaultOptions :: HariharaOptions
defaultOptions = HariharaOptions
  { optsLogLevel = LogInfo
  , optsFiles    = S.empty
  , optsDBPath   = ".harihara.db"
  , optsDBFresh  = False
  }

--------

onOptsLogLevel :: (LogLevel -> LogLevel)
  -> OptionsBuilder
onOptsLogLevel f o = return $ o { optsLogLevel = f $ optsLogLevel o }

setOptsLogLevel :: LogLevel -> OptionsBuilder
setOptsLogLevel ll = onOptsLogLevel $ const ll

--------

onOptsFiles :: (S.Set FilePath -> S.Set FilePath)
  -> OptionsBuilder
onOptsFiles f o = return $ o { optsFiles = f $ optsFiles o }

--------

onOptsDBPath :: (FilePath -> FilePath) -> OptionsBuilder
onOptsDBPath f o = return $ o { optsDBPath = f $ optsDBPath o }

setOptsDBPath :: FilePath -> OptionsBuilder
setOptsDBPath fp = onOptsDBPath $ const fp

--------

onOptsDBFresh :: (Bool -> Bool) -> OptionsBuilder
onOptsDBFresh f o = return $ o { optsDBFresh = f $ optsDBFresh o }

setOptsDBFresh :: Bool -> OptionsBuilder
setOptsDBFresh b = onOptsDBFresh $ const b

-- }}}

-- GetOpt {{{

type OptionsBuilder = HariharaOptions -> IO HariharaOptions

parseOptions :: [String] -> IO HariharaOptions
parseOptions args =
  case getOpt Permute testOpts args of
    (fs, ps, []) -> mkOpts fs ps defaultOptions
    (_ , _ , es) -> mapM_ putStrLn es >> usage

usage :: IO a
usage = do
  putStrLn "Usage:"
  putStr $ usageInfo "harihara [FLAGS] file1 file2 ..." testOpts
  exitFailure

testOpts :: [OptDescr OptionsBuilder]
testOpts =
  [ Option ['l'] ["log"]
      (ReqArg logArg "NUM")
      "Log level: 0/silent, 1/error, 2/warn, 3/info, 4/debug"
  , Option ['d'] ["database"]
      (ReqArg dbArg "FILE")
      "Path to database"
  , Option [] ["fresh-db"]
      (NoArg freshArg)
      "Drop the current database entirely"
  ]

mkOpts :: [OptionsBuilder] -> [FilePath] -> OptionsBuilder
mkOpts fs ps = appBldrs $ fileBldr : fs
  where
  fileBldr = appBldrs $ map fileArg ps

appBldrs :: [OptionsBuilder] -> OptionsBuilder
appBldrs = flip $ foldrM ($)

freshArg :: OptionsBuilder
freshArg = setOptsDBFresh True

-- | Handle a file argument
fileArg :: String -> OptionsBuilder
fileArg fp o = do
  fileExists <- doesFileExist fp
  if fileExists
  then onOptsFiles (S.insert fp) o
  else throwIO $ FileDoesNotExist fp

-- | Handle a log level argument
logArg  :: String -> OptionsBuilder
logArg arg o = case arg of
  "0"      -> setOptsLogLevel LogSilent o
  "silent" -> setOptsLogLevel LogSilent o
  "1"      -> setOptsLogLevel LogError  o
  "error"  -> setOptsLogLevel LogError  o
  "2"      -> setOptsLogLevel LogWarn   o
  "warn"   -> setOptsLogLevel LogWarn   o
  "3"      -> setOptsLogLevel LogInfo   o
  "info"   -> setOptsLogLevel LogInfo   o
  "4"      -> setOptsLogLevel LogDebug  o
  "debug"  -> setOptsLogLevel LogDebug  o
  _        -> throwIO $ NoFlagParse "LogLevel" arg

-- | Handle a DB path argument
dbArg :: String -> OptionsBuilder
dbArg fp o = if isValid fp
  then setOptsDBPath fp o
  else throwIO $ InvalidPath fp

-- }}}

