{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Aeson
-- import           Data.Int                   (Int32)
import           Data.Tagged
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Data.UUID
import qualified Database.PostgreSQL.Simple as PGS
import           Tisch
import           Tisch.Internal.Table

-- import           Data.Kind                  (Type)
-- import           Data.Proxy
import           GHC.TypeLits

import           Lib.Prelude

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
  '[ 'Column "knot_id" 'WD 'R KnotId KnotId
   , 'Column "knot_created_at" 'WD 'R CreationTime CreationTime
   , 'Column "knot_extra_data" 'WD 'R PGJsonb Value
   ]

knot_id :: Knot !> KnotId
knot_id = col (Proxy @"knot_id")

knot_created_at :: Knot !> CreationTime
knot_created_at = col (Proxy @"knot_created_at")

knot_extra_data :: Knot !> Value
knot_extra_data = col (Proxy @"knot_extra_data")

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

path_id :: Path !> KnotId
path_id = col (Proxy @"path_id")

path_source :: Path !> KnotId
path_source = col (Proxy @"path_source")

path_target :: Path !> KnotId
path_target = col (Proxy @"path_target")

---

-- | A convenient type operator for queries.
type (>->) = Query Db

-- | A type operator for lenses that read out of 'HsR' values.
type (!>) a b = Lens' (HsR a) b

-- | Find all knots present in the database.
q_Knot_all :: () >-> PgR Knot
q_Knot_all = query Knot

fetchKnot
  :: forall m.
     (MonadIO m, MonadThrow m)
  => Conn' Db -> (() >-> PgR Knot) -> m [HsR Knot]
fetchKnot = runQuery

connInfo :: PGS.ConnectInfo
connInfo =
  PGS.defaultConnectInfo
  { PGS.connectUser = "lol"
  , PGS.connectPassword = "lol"
  , PGS.connectDatabase = "loldb"
  }

hsi' :: forall (c :: Symbol) x. x -> Tagged c x
hsi' = hsi (C @c)

doInsert :: Conn' Db -> IO ()
doInsert conn = runInsert1 conn Knot kt
  where
    kt =
      mkHsI
        Knot
        (hsi' @"knot_id" WDef)
        (hsi' @"knot_created_at" WDef)
        (hsi'
           @"knot_extra_data"
           (WVal (object [("foo" :: Text) .= ([3, 4 :: Int])])))

runTisch :: IO ()
runTisch = do
  conn <- connect connInfo
  fetchKnot conn q_Knot_all >>=
    traverse_
      (((^. knot_id . to unKnotId) &&&
        (^. knot_created_at . to unCreationTime) &&& (^. knot_extra_data)) |>
       print)
  doInsert conn
  fetchKnot conn q_Knot_all >>=
    traverse_
      (((^. knot_id . to unKnotId) &&&
        (^. knot_created_at . to unCreationTime) &&& (^. knot_extra_data)) |>
       print)

main :: IO ()
main = runTisch
