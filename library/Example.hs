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
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Example where

import           Control.Arrow              (returnA)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Int                   (Int32)
import           Data.Text                  (Text)
import           Data.Time.Clock            (UTCTime)
import           Data.UUID
import qualified Database.PostgreSQL.Simple as PGS
import           Tisch
import           Tisch.Internal.Table

import           Data.Kind                  (Type)
import           Data.Proxy

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
   ]

-- | A convenient type operator for queries.
type (>->) = Query Db

-- | A type operator for lenses that read out of 'HsR' values.
type (!>) a b = Lens' (HsR a) b

-- | Find all knots present in the database.
q_Knot_all :: () >-> PgR Knot
q_Knot_all = query Knot

knot_id :: Knot !> KnotId
knot_id = col (Proxy @"knot_id")

knot_created_at :: Knot !> CreationTime
knot_created_at = col (Proxy @"knot_created_at")

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

doInsert :: Conn' Db -> IO ()
doInsert conn = runInsert1 conn Knot kt
  where kt = mkHsI Knot (hsi (C @"knot_id") WDef) (hsi (C @"knot_created_at") WDef)

runTisch :: IO ()
runTisch = do
  conn <- connect connInfo
  fetchKnot conn q_Knot_all >>=
    traverse_
      (((^. knot_id . to unKnotId) &&& (^. knot_created_at . to unCreationTime)) |>
       print)
  doInsert conn
  fetchKnot conn q_Knot_all >>=
    traverse_
      (((^. knot_id . to unKnotId) &&& (^. knot_created_at . to unCreationTime)) |>
       print)

main :: IO ()
main = runTisch
