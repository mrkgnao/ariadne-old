{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Ariadne.Tables where

import           Control.Arrow              (returnA)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Tagged
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Data.UUID
import qualified Database.PostgreSQL.Simple as PGS
import           Edible
import           GHC.TypeLits
import           Tisch.Internal.Fun         as Tisch
import           Tisch.Internal.Table

import           Lib.Prelude                hiding (like)

import qualified Ariadne.Logging                    as L

import           Ariadne.Ambiguous

-- | An uninhabited type-level database tag.
data Db

----------
-- KnotId
----------

-- | A wrapped version of 'UUID'.
newtype KnotId = KnotId { unKnotId :: UUID }
  deriving (Show, Eq)

-- | A representation of how 'KnotId' wraps 'UUID',
--   for the 'ToKol' instance.
instance Wrapped KnotId where
  type Unwrapped KnotId = UUID
  _Wrapped' = iso unKnotId KnotId

-- | Store 'UUID' in the database as 'PGUuid'.
instance PgTyped KnotId where
  type PgType KnotId = PGUuid

-- | Allow equality tests of 'KnotId' values by lifting from 'UUID'.
instance PgEq KnotId

instance ToKol KnotId KnotId

-- | PG-Haskell type conversion handling, derived from 'Wrapped' 'KnotId'.
instance QueryRunnerColumnDefault PGUuid KnotId where
  queryRunnerColumnDefault = qrcWrapped


----------------
-- CreationTime
----------------

-- | A wrapped version of 'UTCTime'.
newtype CreationTime = CreationTime { unCreationTime :: UTCTime }
  deriving (Show, Eq)

-- | A representation of how 'CreationTime' wraps 'UTCTime',
--   for the 'ToKol' instance.
instance Wrapped CreationTime where
  type Unwrapped CreationTime = UTCTime
  _Wrapped' = iso unCreationTime CreationTime

-- | Store 'UTCTime' in the database as 'PGTimestamptz'.
instance PgTyped CreationTime where
  type PgType CreationTime = PGTimestamptz

-- | Allow equality tests of 'CreationTime' values by lifting from 'UTCTime'.
instance PgEq CreationTime

instance ToKol CreationTime CreationTime

-- | PG-Haskell type conversion handling, derived from 'Wrapped' 'CreationTime'.
instance QueryRunnerColumnDefault PGTimestamptz CreationTime where
  queryRunnerColumnDefault = qrcWrapped


-------------------
-- The Knot table.
-------------------

-- | An uninhabited type tag for the 'Knot' table.
data Knot

-- | A constructor for 'Table' 'Knot'.
data instance Table Knot      = Knot

-- | PostgreSQL table for 'Knot'.
type instance TableName Knot  = "knots"

type instance Database Knot   = Db
type instance SchemaName Knot = "public"

type instance Columns Knot =
  [ 'Column "knot_id" 'WD 'R KnotId KnotId
  , 'Column "knot_created_at" 'WD 'R CreationTime CreationTime
  , 'Column "knot_extra_data" 'WD 'R PGJsonb Value
  ]

knot_id :: ColLens "knot_id" a b b => Lens' a b
knot_id = colLens @"knot_id"

knot_created_at :: ColLens "knot_created_at" a b b => Lens' a b
knot_created_at = colLens @"knot_created_at"

knot_extra_data :: ColLens "knot_extra_data" a b b => Lens' a b
knot_extra_data = colLens @"knot_extra_data"

-------------------
-- The Path table.
-------------------

-- | An uninhabited type tag for the 'Path' table.
data Path

-- | A constructor for 'Table' 'Path'.
data instance Table Path      = Path

-- | Postgres table for 'Path'.
type instance TableName Path  = "paths"

type instance Database Path   = Db
type instance SchemaName Path = "public"

type instance Columns Path =
  [ 'Column "path_id" 'W 'R KnotId KnotId
  , 'Column "path_source" 'W 'R KnotId KnotId
  , 'Column "path_target" 'W 'R KnotId KnotId
  ]

path_id :: ColLens "path_id" a b b => Lens' a b
path_id = colLens @"path_id"

path_source :: ColLens "path_source" a b b => Lens' a b
path_source = colLens @"path_source"

path_target :: ColLens "path_target" a b b => Lens' a b
path_target = colLens @"path_target"

q_Path_all :: Query Db () (PgR Path)
q_Path_all = query Path

q_Path_by_id :: KnotId >-> PgR Path
q_Path_by_id kid = proc () -> do
  k <- query Path -< ()
  restrict -< eq (k ^. path_id) (kol kid)
  returnA -< k


-----------------------
-- The Fulltext table.
-----------------------

-- | An uninhabited type tag for the 'Fulltext' table.
data Fulltext

-- | A constructor for 'Table' 'Fulltext'.
data instance Table Fulltext      = Fulltext

-- | Postgres table for 'Fulltext'.
type instance TableName Fulltext  = "fulltexts"

type instance Database Fulltext   = Db
type instance SchemaName Fulltext = "public"

type instance Columns Fulltext =
  [ 'Column "fulltext_id" 'W 'R KnotId KnotId
  , 'Column "fulltext_contents" 'W 'R PGText Text
  ]

fulltext_id :: ColLens "fulltext_id" a b b => Lens' a b
fulltext_id = colLens @"fulltext_id"

contents :: ColLens "fulltext_contents" a b b => Lens' a b
contents = colLens @"fulltext_contents"

q_Fulltext_all :: Query Db () (PgR Fulltext)
q_Fulltext_all = query Fulltext

q_Fulltext_by_id :: KnotId >-> PgR Fulltext
q_Fulltext_by_id kid = proc () -> do
  k <- query Fulltext -< ()
  restrict -< eq (k ^. fulltext_id) (kol kid)
  returnA -< k

q_Fulltext_by_contents :: Text >-> PgR Fulltext
q_Fulltext_by_contents cts = proc () -> do
  l <- query Fulltext -< ()
  restrict -< like (l ^. contents) (kol ("%" <> cts <> "%"))
  returnA -< l

mkFulltext'
  :: (MonadReader AriadneState m, MonadIO m, MonadThrow m, MonadLogger m)
  => Text -> m KnotId
mkFulltext' s = do
  t <- mkKnot
  mkFulltext t s

mkFulltext
  :: (MonadReader AriadneState m, MonadIO m, MonadThrow m)
  => KnotId -> Text -> m KnotId
mkFulltext t s = do
  c <- asks conn
  runInsertReturning1
    c
    Fulltext
    (^. fulltext_id)
    (mkHsI
       Fulltext
       (hsi' @"fulltext_id" t)
       (hsi' @"fulltext_contents" s))

-------------------
-- The Link table.
-------------------

-- | An uninhabited type tag for the 'Link' table.
data Link

-- | A constructor for 'Table' 'Link'.
data instance Table Link      = Link

-- | Postgres table for 'Link'.
type instance TableName Link  = "links"

type instance Database Link   = Db
type instance SchemaName Link = "public"

type instance Columns Link =
  [ 'Column "link_id" 'W 'R KnotId KnotId
  , 'Column "link_url" 'W 'R PGText Text
  , 'Column "link_title" 'W 'R PGText Text
  ]

q_Link_all :: Query Db () (PgR Link)
q_Link_all = query Link

q_Link_by_id :: KnotId >-> PgR Link
q_Link_by_id kid = proc () -> do
  k <- query Link -< ()
  restrict -< (k ^. link_id) `eq` kol kid
  returnA -< k

q_Link_by_url :: Text >-> PgR Link
q_Link_by_url url = proc () -> do
  l <- query Link -< ()
  restrict -< (l ^. link_url) `like` kol ("%" <> url <> "%")
  returnA -< l

q_Link_by_title :: Text >-> PgR Link
q_Link_by_title title = proc () -> do
  l <- query Link -< ()
  restrict -< (l ^. link_title) `Tisch.ilike` kol ("%" <> title <> "%")
  returnA -< l

link_id :: ColLens "link_id" a b b => Lens' a b
link_id = colLens @"link_id"

link_url :: ColLens "link_url" a b b => Lens' a b
link_url = colLens @"link_url"

link_title :: ColLens "link_title" a b b => Lens' a b
link_title = colLens @"link_title"

createLink
  :: (MonadReader AriadneState m, MonadIO m, MonadThrow m, MonadLogger m)
  => Text -> Text -> m KnotId
createLink url title = do
  s <- mkKnot
  c <- asks conn
  runInsertReturning1
    c
    Link
    (^. link_id)
    (mkHsI
       Link
       (hsi' @"link_id" s)
       (hsi' @"link_url" url)
       (hsi' @"link_title" title))

---

-- | A convenient type operator for queries.
type (>->) a b = a -> Query Db () b

-- | Find all knots present in the database.
q_Knot_all :: Query Db () (PgR Knot)
q_Knot_all = query Knot

q_Knot_by_id :: KnotId >-> PgR Knot
q_Knot_by_id kid = proc () -> do
  k <- query Knot -< ()
  restrict -< eq (k ^. knot_id) (kol kid)
  returnA -< k

newtype AriadneState = AriadneState { conn :: Conn' Db }

newtype AriadneT m a = AriadneT
  { runAriadneT :: ReaderT AriadneState (LoggingT m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AriadneState
             , MonadIO
             , MonadThrow
             , MonadLogger
             )

escapeLabyrinth :: AriadneState -> AriadneT m a -> m a
escapeLabyrinth db a = runLoggingT (runReaderT (runAriadneT a) db) L.logger

fetch
  :: forall a m.
     (MonadIO m, MonadThrow m, MonadReader AriadneState m, MonadLogger m, _)
  => Query Db () (PgR a) -> m [HsR a]
fetch q = do
  c <- asks conn
  recs :: [HsR a] <- runQuery c q
  logInfoN ("Fetched " <> sshow (length recs) <> " record(s)")
  pure recs

connInfo :: PGS.ConnectInfo
connInfo =
  PGS.defaultConnectInfo
  { PGS.connectUser = "lol"
  , PGS.connectPassword = "lol"
  , PGS.connectDatabase = "loldb"
  }

hsi' :: forall (c :: Symbol) x. x -> Tagged c x
hsi' = hsi (C @c)

mkKnot
  :: (MonadReader AriadneState m, MonadIO m, MonadThrow m, MonadLogger m)
  => m KnotId
mkKnot = do
  c <- asks conn
  kid <- runInsertReturning1 c Knot (^. knot_id) kt
  logDebugN ("Created knot with id " <> sshow kid)
  pure kid
  where
    kt =
      mkHsI Knot
        (hsi' @"knot_id" WDef)
        (hsi' @"knot_created_at" WDef)
        (hsi' @"knot_extra_data" WDef)

mkPath
  :: (MonadReader AriadneState m, MonadIO m, MonadThrow m, MonadLogger m)
  => KnotId -> KnotId -> m KnotId
mkPath s d = do
  t <- mkKnot
  c <- asks conn
  runInsertReturning1
    c
    Path
    (^. path_id)
    (mkHsI
       Path
       (hsi' @"path_id" d)
       (hsi' @"path_source" s)
       (hsi' @"path_target" t))


linkSearchLoop :: AriadneT IO ()
linkSearchLoop = do
  putText "\nEnter search query: "
  str <- liftIO getLine
  fetch (q_Link_by_title str) >>= traverse_ (^. link_title . to putTextLn)
  linkSearchLoop

runTisch :: IO ()
runTisch = do
  conn <- connect connInfo
  escapeLabyrinth (AriadneState conn) linkSearchLoop

main :: IO ()
main = runTisch
