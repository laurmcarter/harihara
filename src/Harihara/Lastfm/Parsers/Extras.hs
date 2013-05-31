{-# LANGUAGE OverloadedStrings #-}

module Harihara.Lastfm.Parsers.Extras where

import Data.Aeson.Types

import Control.Applicative
import Control.Monad
import Data.Maybe (listToMaybe)
import Data.Text

toBool :: Parser Int -> Parser Bool
toBool = fmap (0 /=)

(.::) :: (FromJSON a) => Object -> (Text,Text) -> Parser [a]
r .:: (as,a) = r .: as >>= (.: a) >>= oneOrMore

(@@) :: (FromJSON a) => Object -> Text -> Parser a
o @@ t = o .: "@attr" >>= (.: t)

(@@#) :: (Read a) => Object -> Text -> Parser a
o @@# t = o .: "@attr" >>= (.:# t)

(.:#) :: (Read a) => Object -> Text -> Parser a
o .:# t = do
  s <- o .: t
  case maybeRead s of
    Nothing -> mzero
    Just i  -> return i

oneOrMore :: (FromJSON a) => Value -> Parser [a]
oneOrMore v = ((:[]) <$> parseJSON v) `mplus` parseJSON v

couldBeEither :: (FromJSON a,FromJSON b) => Value -> Parser (Either a b)
couldBeEither v = (Left <$> parseJSON v) `mplus` (Right <$> parseJSON v)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

(>>=?) :: (Functor m, Monad m) => m (Maybe a) -> (a -> m b) -> m (Maybe b)
m >>=? f = do
  r <- m
  case r of
    Just a -> Just <$> f a
    Nothing -> return Nothing

(@@?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
o @@? t = o .:? "@attr" >>= maybe (return Nothing) (fmap Just . (.: t))

numStr :: Parser (Maybe String) -> Parser (Maybe Int)
numStr m = do
  ma <- m
  return (ma >>= maybeRead)

