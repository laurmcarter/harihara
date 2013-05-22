
module Harihara.Utils where

import Data.Char
import qualified Data.Text as T

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (return ()) f ma

withEither :: Either a b -> (a -> c) -> (b -> c) -> c
withEither m fa fb = either fa fb m

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe m d f = maybe d f m

capitalize :: T.Text -> T.Text
capitalize = T.tail . cap
  where
  cap = flip T.scanl ' ' $ \c1 c2 -> 
    if isSpace c1
    then toUpper c2
    else c2

spaceToUnderscore :: T.Text -> T.Text
spaceToUnderscore = T.map $ \c ->
  if isSpace c
  then '_'
  else c

underscoreToSpace :: T.Text -> T.Text
underscoreToSpace = T.map $ \c ->
  if c == '_'
  then ' '
  else c

