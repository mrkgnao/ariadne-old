{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Ariadne.Models.Memory where

import           Control.Arrow          (returnA)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString        as BS
import           Data.Tagged
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime)
import           Data.UUID
import           Edible
import           Edible.Internal.Table
import           GHC.TypeLits

import           Lib.Prelude            hiding (like)

import           Ariadne.Ambiguous
import           Ariadne.Database
import           Ariadne.Models.Knot


-------------------
-- The Card table.
-------------------

-- | An uninhabited type tag for the 'Card' table.
data Card

-- | A constructor for 'Table' 'Card'.
data instance Table Card      = Card

-- | Postgres table for 'Card'.
type instance TableName Card  = "cards"

type instance Database Card   = Db
type instance SchemaName Card = "public"

type instance Columns Card =
  [ 'Column "card_id" 'WD 'R KnotId KnotId
  , 'Column "front_text" 'WD 'R PGText Text
  , 'Column "back_text" 'WD 'R PGText Text
  ]


-----------
-- DueDate
-----------

-- | A wrapped version of 'UTCTime'.
newtype DueDate = DueDate { unDueDate :: UTCTime }
  deriving (Show, Eq)

-- | A representation of how 'DueDate' wraps 'UTCTime',
--   for the 'ToKol' instance.
instance Wrapped DueDate where
  type Unwrapped DueDate = UTCTime
  _Wrapped' = iso unDueDate DueDate

-- | Store 'UTCTime' in the database as 'PGTimestamptz'.
instance PgTyped DueDate where
  type PgType DueDate = PGTimestamptz

-- | Allow equality tests of 'DueDate' values by lifting from 'UTCTime'.
instance PgEq DueDate

instance ToKol DueDate DueDate

-- | PG-Haskell type conversion handling, derived from 'Wrapped' 'DueDate'.
instance QueryRunnerColumnDefault PGTimestamptz DueDate where
  queryRunnerColumnDefault = qrcWrapped


------------------
-- EasinessFactor
------------------

-- | A wrapped version of 'Double'.
newtype EasinessFactor = EasinessFactor { unEasinessFactor :: Double }
  deriving (Show, Eq)

-- | A representation of how 'EasinessFactor' wraps 'Double',
--   for the 'ToKol' instance.
instance Wrapped EasinessFactor where
  type Unwrapped EasinessFactor = Double
  _Wrapped' = iso unEasinessFactor EasinessFactor

-- | Store 'Double' in the database as 'PGFloat8'.
instance PgTyped EasinessFactor where
  type PgType EasinessFactor = PGFloat8

-- | Allow equality tests of 'EasinessFactor' values by lifting from 'Double'.
instance PgEq EasinessFactor

instance ToKol EasinessFactor EasinessFactor

-- | PG-Haskell type conversion handling, derived from 'Wrapped' 'EasinessFactor'.
instance QueryRunnerColumnDefault PGFloat8 EasinessFactor where
  queryRunnerColumnDefault = qrcWrapped


---------------------
-- The Memory table.
---------------------

-- | An uninhabited type tag for the 'Memory' table.
data Memory

-- | A constructor for 'Table' 'Memory'.
data instance Table Memory      = Memory

-- | Postgres table for 'Memory'.
type instance TableName Memory  = "memories"

type instance Database Memory   = Db
type instance SchemaName Memory = "public"

type instance Columns Memory =
  [ 'Column "memory_id" 'WD 'R KnotId KnotId
  , 'Column "easiness_factor" 'WD 'R EasinessFactor EasinessFactor
  , 'Column "repetition_interval" 'WD 'R PGFloat8 Double
  , 'Column "repetition_count" 'WD 'R PGInt4 Int32
  , 'Column "next_due" 'WD 'R DueDate DueDate
  ]

memory_id :: ColLens "memory_id" a b b => Lens' a b
memory_id = colLens @"memory_id"

q_Memory_all :: Query Db () (PgR Memory)
q_Memory_all = query Memory

q_Memory_by_id :: KnotId >-> PgR Memory
q_Memory_by_id kid = proc () -> do
  m <- query Memory -< ()
  restrict -< eq (m ^. memory_id) (kol kid)
  returnA -< m
