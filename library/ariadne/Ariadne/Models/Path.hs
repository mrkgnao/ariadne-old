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

module Ariadne.Models.Path where

import           Control.Arrow              (returnA)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString            as BS
import           Data.Tagged
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Encoding.Error   as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Data.Time.Clock            (UTCTime)
import           Data.UUID
import qualified Database.PostgreSQL.Simple as PGS
import           Edible
import           GHC.TypeLits
import           Tisch.Internal.Fun         as Tisch
import           Tisch.Internal.Table

import           Lib.Prelude                hiding (like)

import           Ariadne.Ambiguous
import           Ariadne.Database
import qualified Ariadne.Logging            as L
import           Ariadne.Models.Knot

import qualified Network.Wreq               as W



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
