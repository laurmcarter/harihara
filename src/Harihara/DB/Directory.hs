
module Harihara.DB.Directory where

import Control.Monad
import System.Directory
import System.FilePath

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn fp = do
  fs <- getDirectoryContents fp
  fss <- forM fs $ \f -> if all (== '.') f 
    then return []
    else do
      let f' = fp </> f
      isDir <- doesDirectoryExist f'
      if isDir
      then allFilesIn f'
      else return [f']
  return $ concat fss

