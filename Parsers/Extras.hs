{-# LANGUAGE OverloadedStrings #-}

module Parsers.Extras where

import Data.Aeson.Types

import Control.Applicative
import Control.Monad
import Data.Text

toBool :: Parser Int -> Parser Bool
toBool = fmap (0 /=)

(>>=?) :: (Functor m, Monad m) => m (Maybe a) -> (a -> m b) -> m (Maybe b)
m >>=? f = do
  r <- m
  case r of
    Just a -> Just <$> f a
    Nothing -> return Nothing

(@@) :: (FromJSON a) => Object -> Text -> Parser a
o @@ t = o .: "@attr" >>= (.: t)

(@@?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
o @@? t = o .:? "@attr" >>= maybe (return Nothing) (fmap Just . (.: t))

oneOrMore :: (FromJSON a) => Value -> Parser [a]
oneOrMore v = ((:[]) <$> parseJSON v) `mplus` parseJSON v

couldBeEither :: (FromJSON a,FromJSON b) => Value -> Parser (Either a b)
couldBeEither v = (Left <$> parseJSON v) `mplus` (Right <$> parseJSON v)

