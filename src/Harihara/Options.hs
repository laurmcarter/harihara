{-# LANGUAGE TemplateHaskell #-}

module Harihara.Options 
  ( parseOptions
  , parseOptionsWith
  , HariharaOptions (..)
  ) where

import Data.List (isPrefixOf)
import qualified Data.Set as S

import Harihara.Log

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

-- Handlers {{{

type OptionsBuilder = HariharaOptions -> Maybe HariharaOptions
type LongFlag = String
type ShortFlag = String
type Arg  = String
type Usage = String

type FlagHandler = (LongFlag,ShortFlag,Usage,Arg -> OptionsBuilder)

defaultFlagHandlers :: [FlagHandler]
defaultFlagHandlers =
  [ ( "log"      , "l"  , "[[0-4]|silent|error|warn|info|debug]" , handleLogLevel )
  , ( "database" , "db" , "<path/to/db>"                         , handleDBPath   )
  , ( "fresh-db" , "f"  , ""                                     , handleDBFresh  )
  ]

handleLogLevel :: Arg -> OptionsBuilder
handleLogLevel ll opts = do
  lvl <- parsedLevel
  setOptsLogLevel lvl opts
  where
  parsedLevel = case ll of
    "silent" -> Just LogSilent
    "error" -> Just LogError
    "warn"  -> Just LogWarn
    "info"  -> Just LogInfo
    "debug" -> Just LogDebug
    "0"     -> Just LogSilent
    "1"     -> Just LogError
    "2"     -> Just LogWarn
    "3"     -> Just LogInfo
    "4"     -> Just LogDebug
    _       -> Nothing

handleDBPath :: Arg -> OptionsBuilder
handleDBPath fp = setOptsDBPath fp

handleDBFresh :: Arg -> OptionsBuilder
handleDBFresh _ = setOptsDBFresh True

handleFile  :: Arg -> OptionsBuilder
handleFile f = onOptsFiles $ S.insert f

-- }}}

-- Parsing Options {{{

parseOptions :: [String] -> Either Usage HariharaOptions
parseOptions = parseOptionsWith defaultFlagHandlers

parseOptionsWith :: [FlagHandler] -> [String] -> Either Usage HariharaOptions
parseOptionsWith hs args = case loop args of
  Just opts -> Right opts
  Nothing   -> Left $ mkUsage hs
  where
  loop as = case as of
    ('-':'-':flag):arg:rest -> longFlag  hs flag arg =<< loop rest
    ('-':flag):rest         -> shortFlag hs flag     =<< loop rest
    f:rest                  -> handleFile f          =<< loop rest
    []                      -> return defaultOptions

shortFlag :: [FlagHandler] -> String -> OptionsBuilder
shortFlag hs fa = loop hs
  where
  loop l = case l of
    (_,sf,_,h):l'
      | sf `isPrefixOf` fa -> h $ splitPrefix sf fa
      | otherwise          -> loop l'
    []                     -> return

longFlag :: [FlagHandler] -> LongFlag -> Arg -> OptionsBuilder
longFlag hs f a = loop hs
  where
  loop l = case l of
    (lf,_,_,h):l'
      | f == lf   -> h a
      | otherwise -> loop l'
    []            -> return

mkUsage :: [FlagHandler] -> Usage
mkUsage = unwords . (map $ \(lf,sf,u,_) ->
  "[" ++ long lf ++ "|" ++ short sf ++ "]"++u)
  where
  long lf = "--" ++ lf ++ " "
  short sf = "-" ++ sf

splitPrefix :: String -> String -> String
splitPrefix prf s = case (prf,s) of
  (c:prf',c':s')
    | c == c'   -> splitPrefix prf' s'
    | otherwise -> s
  ([],rest)     -> rest
  _             -> ""

-- }}}

