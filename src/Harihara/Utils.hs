
module Harihara.Utils where

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (return ()) f ma

withEither :: Either a b -> (a -> c) -> (b -> c) -> c
withEither m fa fb = either fa fb m

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe m d f = maybe d f m

