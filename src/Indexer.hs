{-# LANGUAGE OverloadedStrings #-}

module Indexer where

import Control.Applicative
import Data.Configurator as Cfg
import Data.Configurator.Types
import Data.Text

main :: IO ()
main = do
  cfg <- build =<< load configFiles
  putStrLn $ show cfg

