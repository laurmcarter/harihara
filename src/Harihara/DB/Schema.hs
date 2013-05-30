
module Harihara.DB.Schema where

import Database.SQLite

import Control.Applicative
import qualified Data.Text as T

-- DB Schema {{{

songTable :: SQLTable
songTable = Table
  { tabName        = "songs"
  , tabColumns     =
    [ stringCol      "title"
    , stringCol      "artist"
    , stringCol      "album"
    , maybeStringCol "mbid"
    , stringCol      "file"
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

-- }}}

-- SongRow {{{

class IsRow r where
  toRow :: r -> Row Value
  fromRow :: Row Value -> Maybe r

data SongRow = SongRow
  { songRowTitle  :: T.Text
  , songRowArtist :: T.Text
  , songRowAlbum  :: T.Text
  , songRowMBID   :: Maybe T.Text
  , songRowFile   :: FilePath
  } deriving (Show)

instance IsRow SongRow where
  toRow (SongRow tl art alb mb fp) =
    [ (":title"  , fromText tl)
    , (":artist" , fromText art)
    , (":album"  , fromText alb)
    , (":mbid"   , fromMaybeText mb)
    , (":file"   , fromString fp)
    ]
  fromRow rs = SongRow                <$>
    (toText   =<< lookup "title"  rs) <*>
    (toText   =<< lookup "artist" rs) <*>
    (toText   =<< lookup "album"  rs) <*>
    (toMaybeText =<< lookup "mbid" rs) <*>
    (toString =<< lookup "file"   rs)

-- }}}

-- Helpers {{{

type SQLColumn = Column SQLType

int :: Value -> Maybe Int
int (Int i) = return $ fromEnum i
int _ = Nothing

toText :: Value -> Maybe T.Text
toText (Text t) = return $ T.pack t
toText _ = Nothing

toMaybeText :: Value -> Maybe (Maybe T.Text)
toMaybeText (Text t) = return $ Just $ T.pack t
toMaybeText Null = return Nothing
toMaybeText _ = Nothing

fromText :: T.Text -> Value
fromText = Text . T.unpack

fromMaybeText :: Maybe T.Text -> Value
fromMaybeText = maybe Null (Text . T.unpack)

toString :: Value -> Maybe String
toString (Text t) = return t
toString _ = Nothing

fromString :: String -> Value
fromString = Text

-- }}}

