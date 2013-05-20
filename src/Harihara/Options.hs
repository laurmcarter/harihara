{-# LANGUAGE TemplateHaskell #-}

module Harihara.Options where

import Control.Lens

import Data.List (isPrefixOf)
import qualified Data.Set as S

import Harihara.Log
import Harihara.Utils

data HariharaOptions = HariharaOptions
  { _optsLogLevel :: LogLevel
  , _optsFiles    :: S.Set FilePath
  } deriving (Show)

makeLenses ''HariharaOptions

defaultFlagHandlers :: [FlagHandler]
defaultFlagHandlers =
  [ ( "log" , "l" , handleLogLevel )
  ]

defaultOptions :: HariharaOptions
defaultOptions = HariharaOptions
  { _optsLogLevel = LogError
  , _optsFiles    = S.empty
  }

parseOptions :: [String] -> HariharaOptions
parseOptions = parseOptionsWith defaultFlagHandlers

parseOptionsWith :: [FlagHandler] -> [String] -> HariharaOptions
parseOptionsWith hs args = case args of
  ('-':'-':flag):arg:rest -> longFlag  hs flag arg $ parseOptionsWith hs rest
  ('-':flag):rest         -> shortFlag hs flag     $ parseOptionsWith hs rest
  f:rest                  -> handleFile f          $ parseOptionsWith hs rest
  _                       -> defaultOptions

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

handleLogLevel :: String -> OptionsBuilder
handleLogLevel ll =
  withMaybe parsedLevel id
    (optsLogLevel .~)
  where
  parsedLevel = case ll of
    "warn"  -> Just LogWarn
    "info"  -> Just LogInfo
    "debug" -> Just LogDebug
    "2"     -> Just LogWarn
    "3"     -> Just LogInfo
    "4"     -> Just LogDebug
    _       -> Nothing

handleFile  :: String -> HariharaOptions -> HariharaOptions
handleFile f = optsFiles %~ (S.insert f)

splitPrefix :: String -> String -> String
splitPrefix prf s = case (prf,s) of
  (c:prf',c':s')
    | c == c'   -> splitPrefix prf' s'
    | otherwise -> s
  ([],rest)     -> rest
  _             -> ""

