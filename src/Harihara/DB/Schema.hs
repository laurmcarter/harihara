{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Harihara.DB.Schema where

import Database.SQLite
import Data.Time.Calendar

import Control.Applicative
import qualified Data.Text as T

-- DB Schema {{{

hariharaSchema :: [SQLTable]
hariharaSchema =
  [ trackTable
  , artistTable
  ]

trackTable :: SQLTable
trackTable = Table
  { tabName        = "tracks"
  , tabColumns     =
    [ stringCol      "title"
    , stringCol      "artist"
    , stringCol      "album"
    , stringCol      "file"
    , maybeStringCol "mbid"
    , maybeStringCol "url"
    , maybeStringCol "tags"
    , maybeIntCol    "release"
    , maybeStringCol "images"
    ]
  , tabConstraints = []
  }

artistTable :: SQLTable
artistTable = Table
  { tabName = "artists"
  , tabColumns =
    [ stringCol "name"
    , stringCol "mbid"
    , stringCol "similar"
    , stringCol "tags"
    ]
  , tabConstraints = []
  }

stringCol :: String -> SQLColumn
stringCol name = Column
  { colName = name
  , colType = SQLVarChar 1024
  , colClauses = [IsNullable False]
  }

maybeStringCol :: String -> SQLColumn
maybeStringCol name = Column
  { colName = name
  , colType = SQLVarChar 1024
  , colClauses = [IsNullable True]
  }

maybeIntCol :: String -> SQLColumn
maybeIntCol name = Column
  { colName = name
  , colType = SQLInt NORMAL True False
  , colClauses = [IsNullable True]
  }

class IsRow r where
  toRow :: r -> Row Value
  fromRow :: Row Value -> Maybe r

-- }}}

-- DBTrack {{{

data DBTrack = DBTrack
  { dbTrackTitle   :: T.Text
  , dbTrackArtist  :: T.Text
  , dbTrackAlbum   :: T.Text
  , dbTrackFile    :: FilePath
  , dbTrackMBID    :: Maybe T.Text
  , dbTrackURL     :: Maybe T.Text
  , dbTrackTags    :: Maybe [T.Text]
  , dbTrackRelease :: Maybe Day
  , dbTrackImages  :: Maybe [T.Text]
  } deriving (Show)

instance IsRow DBTrack where
  toRow t = zip cols vals
    where
    cols = map (\c -> ':' : colName c) (tabColumns trackTable)
    vals =
      [ fromText           $ dbTrackTitle   t
      , fromText           $ dbTrackArtist  t
      , fromText           $ dbTrackAlbum   t
      , fromString         $ dbTrackFile    t
      , fromMaybeText      $ dbTrackMBID    t
      , fromMaybeText      $ dbTrackURL     t
      , fromMaybeTextList  $ dbTrackTags    t
      , fromMaybeDay       $ dbTrackRelease t
      , fromMaybeTextList  $ dbTrackImages  t
      ]
  fromRow rs = DBTrack                        <$>
    (toText          =<< lookup "title"   rs) <*>
    (toText          =<< lookup "artist"  rs) <*>
    (toText          =<< lookup "album"   rs) <*>
    (toString        =<< lookup "file"    rs) <*>
    (toMaybeText     =<< lookup "mbid"    rs) <*>
    (toMaybeText     =<< lookup "url"     rs) <*>
    (toMaybeTextList =<< lookup "tags"    rs) <*>
    (toMaybeDay      =<< lookup "release" rs) <*>
    (toMaybeTextList =<< lookup "images"  rs)

-- }}}

-- DBArtist {{{

data DBArtist = DBArtist
  { dbArtistName    :: T.Text
  , dbArtistMBID    :: T.Text
  , dbArtistSimilar :: [T.Text]
  , dbArtistTags    :: [T.Text]
  } deriving (Show)

instance IsRow DBArtist where
  toRow a = zip cols vals
   where
   cols = map (\c -> ':' : colName c) (tabColumns artistTable)
   vals =
     [ fromText     $ dbArtistName a
     , fromText     $ dbArtistMBID a
     , fromTextList $ dbArtistSimilar a
     , fromTextList $ dbArtistSimilar a
     ]
  fromRow rs = DBArtist                  <$>
    (toText     =<< lookup "name"    rs) <*>
    (toText     =<< lookup "mbid"    rs) <*>
    (toTextList =<< lookup "similar" rs) <*>
    (toTextList =<< lookup "tags"    rs)

-- }}}

-- Helpers {{{

type SQLColumn = Column SQLType

int :: Value -> Maybe Int
int (Int i) = return $ fromEnum i
int _ = Nothing

toText :: Value -> Maybe T.Text
toText (Text t) = return $ T.pack t
toText _ = Nothing

toTextList :: Value -> Maybe [T.Text]
toTextList (Text t) = return $ map T.pack $ splitAll t
toTextList _        = Nothing

toMaybeTextList :: Value -> Maybe (Maybe [T.Text])
toMaybeTextList (Text t) = return $ Just $ map T.pack $ splitAll t
toMaybeTextList Null = return Nothing
toMaybeTextList _ = Nothing

toMaybeText :: Value -> Maybe (Maybe T.Text)
toMaybeText (Text t) = return $ Just $ T.pack t
toMaybeText Null = return Nothing
toMaybeText _ = Nothing

toMaybeDay :: Value -> Maybe (Maybe Day)
toMaybeDay (Int i) = return $ Just $ transEnum i
toMaybeDay Null = return Nothing
toMaybeDay _ = Nothing

fromText :: T.Text -> Value
fromText = Text . T.unpack

fromTextList :: [T.Text] -> Value
fromTextList = fromText . T.intercalate "|"

fromMaybeText :: Maybe T.Text -> Value
fromMaybeText = maybe Null (Text . T.unpack)

fromMaybeTextList :: Maybe [T.Text] -> Value
fromMaybeTextList (Just ts) = fromText $ T.intercalate "|" ts
fromMaybeTextList Nothing = Null

fromMaybeDay :: Maybe Day -> Value
fromMaybeDay (Just d) = Int $ transEnum d
fromMaybeDay Nothing = Null

toString :: Value -> Maybe String
toString (Text t) = return t
toString _ = Nothing

fromString :: String -> Value
fromString = Text

transEnum :: (Enum a, Enum b) => a -> b
transEnum = toEnum . fromEnum

splitAll :: String -> [String]
splitAll t = if null t
  then []
  else let (s,rest) = break (== '|') t in
    s : splitAll (drop 1 rest)

-- }}}

