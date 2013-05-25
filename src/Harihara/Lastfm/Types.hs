{-# LANGUAGE DataKinds #-}

module Harihara.Lastfm.Types where

import Network.Lastfm

type LfmRequest = Request JSON Send
type LfmRequestAuth = Request JSON Sign

type KeyedRequest = LfmRequest (APIKey -> Ready)

data LastfmEnv = LastfmEnv
  { getApiKey   :: LfmRequest APIKey
  , signRequest :: LfmRequestAuth Ready -> LfmRequest Ready
  }

