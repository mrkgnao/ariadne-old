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

module Ariadne.Models.Link where

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

import           Lib.Prelude                hiding (like, ilike)

import           Ariadne.Ambiguous
import           Ariadne.Database
import qualified Ariadne.Logging            as L
import           Ariadne.Models.Knot
import           Ariadne.AriadneT

import qualified Network.Wreq               as W


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
  restrict -< (l ^. link_title) `ilike` kol ("%" <> title <> "%")
  returnA -< l

link_id :: ColLens "link_id" a b b => Lens' a b
link_id = colLens @"link_id"

link_url :: ColLens "link_url" a b b => Lens' a b
link_url = colLens @"link_url"

link_title :: ColLens "link_title" a b b => Lens' a b
link_title = colLens @"link_title"
