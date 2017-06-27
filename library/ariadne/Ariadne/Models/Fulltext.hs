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

module Ariadne.Models.Fulltext where

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
import           Ariadne.AriadneT

import qualified Network.Wreq               as W

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
