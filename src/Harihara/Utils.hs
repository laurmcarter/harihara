
module Harihara.Utils where

withEither :: Either a b -> (a -> c) -> (b -> c) -> c
withEither m fa fb = either fa fb m

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe m d f = maybe d f m

