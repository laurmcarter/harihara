{-# LANGUAGE OverloadedStrings #-}

module Indexer where

import Control.Applicative
import Data.Configurator as Cfg
import Data.Configurator.Types
import Data.Text

data IndexerConfig = ICfg
  { foo :: String
  , bar :: Int
  } deriving Show

configFiles :: [Worth FilePath]
configFiles =
  [ Required "$(HOME)/.harihara"
  ]

main :: IO ()
main = do
  cfg <- build =<< load configFiles
  putStrLn $ show cfg

