{-# LANGUAGE OverloadedStrings #-}

module Harihara.DB where

import Database.SQLite
import MonadLib

import Control.Applicative
import Data.Text

-- DB Monad {{{

newtype DB a = DB { unDB :: ReaderT DBEnv IO a }

instance Functor DB where
  fmap f (DB m) = DB $ fmap f m

instance Monad DB where
  return = DB . return
  (DB m) >>= f = DB $ m >>= unDB . f

instance Applicative DB where
  pure = return
  (<*>) = ap

runDB :: DBEnv -> DB a -> IO a
runDB env m = runReaderT env $ unDB m

io :: IO a -> DB a
io = DB . inBase

-- }}}

-- DBEnv {{{

data DBEnv = DBEnv
  { dbConn :: SQLiteHandle
  }

getEnv :: DB DBEnv
getEnv = DB ask

fromEnv :: (DBEnv -> a) -> DB a
fromEnv = DB . asks

getConn :: DB SQLiteHandle
getConn = fromEnv dbConn

-- }}}

-- DBOpts {{{

data DBOpts = DBOpts
  { dbPath  :: FilePath
  , dbFresh :: Bool
  }

-- }}}

-- Bracketing {{{

openDB :: FilePath -> IO SQLiteHandle
openDB fp = openConnection fp

closeDB :: DB ()
closeDB = do
  conn <- getConn
  io $ closeConnection conn

withDB :: FilePath -> DB a -> IO a
withDB fp m = do
  conn <- openDB fp
  runDB (DBEnv conn) $ do
    a <- m
    closeDB
    return a

-- }}}

-- DB Wrappers {{{



-- }}}

-- IsRow class {{{

class IsRow r where
  toRow :: r -> Row Value
  fromRow :: Row Value -> Maybe r

instance IsRow SongRow where
  toRow (SongRow sid tl art alb mb fp) =
    [ ("id"     , Int  $ toEnum sid)
    , ("title"  , Text $ unpack tl)
    , ("artist" , Text $ unpack art)
    , ("album"  , Text $ unpack alb)
    , ("mbid"   , Text $ unpack mb)
    , ("file"   , Text $ unpack fp)
    ]
  fromRow rs = SongRow <$>
    (int =<<  lookup "id"     rs) <*>
    (text =<< lookup "title"  rs) <*>
    (text =<< lookup "artist" rs) <*>
    (text =<< lookup "album"  rs) <*>
    (text =<< lookup "mbid"   rs) <*>
    (text =<< lookup "file"   rs)

-- }}}

-- DB Schema {{{

songTable :: SQLTable
songTable = Table
  { tabName        = "songs"
  , tabColumns     =
    [ idCol
    , stringCol "title"
    , stringCol "artist"
    , stringCol "album"
    , stringCol "mbid"
    , stringCol "file"
    ]
  , tabConstraints = []
  }

data SongRow = SongRow
  { songRowId     :: Int
  , songRowTitle  :: Text
  , songRowArtist :: Text
  , songRowAlbum  :: Text
  , songRowMBID   :: Text
  , songRowFile   :: Text
  }

idCol :: SQLColumn
idCol = Column
  { colName = "id"
  , colType = SQLInt NORMAL True False
  , colClauses =
    [ PrimaryKey True
    , Unique
    , IsNullable False
    ]
  }

stringCol :: String -> SQLColumn
stringCol name = Column
  { colName = name
  , colType = SQLVarChar 1024
  , colClauses = [IsNullable False]
  }

type SQLColumn = Column SQLType

int :: Value -> Maybe Int
int (Int i) = Just $ fromEnum i
int _ = Nothing

text :: Value -> Maybe Text
text (Text t) = Just $ pack t
text _ = Nothing

-- }}}

