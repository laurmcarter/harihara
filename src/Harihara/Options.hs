{-# LANGUAGE TemplateHaskell #-}

module Harihara.Options 
  ( parseOptions
  , parseOptionsWith
  , HariharaOptions (..)
  ) where

import Data.List (isPrefixOf)
import qualified Data.Set as S

import Harihara.Log
import Harihara.Utils

-- HariharaOptions {{{

data HariharaOptions = HariharaOptions
  { optsLogLevel :: LogLevel
  , optsFiles    :: S.Set FilePath
  } deriving (Show)

defaultOptions :: HariharaOptions
defaultOptions = HariharaOptions
  { optsLogLevel = LogInfo
  , optsFiles    = S.empty
  }

setOptsLogLevel :: LogLevel -> HariharaOptions -> HariharaOptions
setOptsLogLevel = onOptsLogLevel . const

onOptsLogLevel :: (LogLevel -> LogLevel)
  -> HariharaOptions -> HariharaOptions
onOptsLogLevel f o = o { optsLogLevel = f $ optsLogLevel o }

onOptsFiles :: (S.Set FilePath -> S.Set FilePath)
  -> HariharaOptions -> HariharaOptions
onOptsFiles f o = o { optsFiles = f $ optsFiles o }

-- }}}

-- Handlers {{{

defaultFlagHandlers :: [FlagHandler]
defaultFlagHandlers =
  [ ( "log" , "l" , handleLogLevel )
  ]

handleLogLevel :: String -> OptionsBuilder
handleLogLevel ll =
  withMaybe parsedLevel id setOptsLogLevel
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

handleFile  :: String -> HariharaOptions -> HariharaOptions
handleFile f = onOptsFiles $ S.insert f

-- }}}

-- Parsing Options {{{

parseOptions :: [String] -> HariharaOptions
parseOptions = parseOptionsWith defaultFlagHandlers

parseOptionsWith :: [FlagHandler] -> [String] -> HariharaOptions
parseOptionsWith hs args = case args of
  ('-':'-':flag):arg:rest -> longFlag  hs flag arg $ parseOptionsWith hs rest
  ('-':flag):rest         -> shortFlag hs flag     $ parseOptionsWith hs rest
  f:rest                  -> handleFile f          $ parseOptionsWith hs rest
  _                       -> defaultOptions

-- }}}

-- Parsing Options {{{

type OptionsBuilder = HariharaOptions -> HariharaOptions

type LongFlag = String
type ShortFlag = String
type Arg  = String

type FlagHandler = (LongFlag,ShortFlag,Arg -> OptionsBuilder)

shortFlag :: [FlagHandler] -> String -> OptionsBuilder
shortFlag hs fa = loop hs
  where
  loop l = case l of
    (_,sf,h):l'
      | sf `isPrefixOf` fa -> h $ splitPrefix sf fa
      | otherwise          -> loop l'
    []                     -> id

longFlag :: [FlagHandler] -> LongFlag -> Arg -> OptionsBuilder
longFlag hs f a = loop hs
  where
  loop l = case l of
    (lf,_,h):l'
      | f == lf   -> h a
      | otherwise -> loop l'
    []            -> id

splitPrefix :: String -> String -> String
splitPrefix prf s = case (prf,s) of
  (c:prf',c':s')
    | c == c'   -> splitPrefix prf' s'
    | otherwise -> s
  ([],rest)     -> rest
  _             -> ""

-- }}}

