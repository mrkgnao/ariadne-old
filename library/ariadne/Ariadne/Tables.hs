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
import           Edible.Internal.Table

import           Lib.Prelude                hiding (like)

import           Ariadne.Ambiguous
import           Ariadne.Database
import qualified Ariadne.Logging            as L

import           Ariadne.Models.Knot
import           Ariadne.Models.Link
import           Ariadne.Models.Path
import           Ariadne.Models.Fulltext
import           Ariadne.AriadneT

import qualified Network.Wreq               as W

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

mkKnot'
  :: (MonadReader AriadneState m, MonadIO m, MonadThrow m, MonadLogger m)
  => m ()
mkKnot' = do
  c <- asks conn
  runInsert1 c Knot kt
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
  unless
    (str == "\\quit")
    (do fetch @Link (q_Link_by_title str) >>=
          traverse_
            (\l ->
               (do l ^. link_title |$ ("Title: " <>) |> logInfoN
                   l ^. link_url   |$ ("URL: " <>)   |> logInfoN
                   updateFulltext (l ^. link_id)))
        linkSearchLoop)

runTisch :: IO ()
runTisch = do
  conn <- connect connInfo
  escapeLabyrinth
    (AriadneState conn)
    (do mkKnot'
        linkSearchLoop)

updateFulltext :: KnotId -> AriadneT IO ()
updateFulltext kid =
  fetch (q_Link_by_id kid) >>= \case
    [lnk] -> do
      let l = lnk |$ view link_url
      logDebugN ("Fetching " <> l)
      page <- l |$ T.unpack
                    |> W.get |> liftIO
      let
        pageText = page
                |$ view W.responseBody
                |> TL.decodeUtf8With T.ignore
                |> TL.toStrict
      logInfoN ("Fetched page: " <> tshow (T.length pageText) <> " bytes")
    _ -> pure ()

getLink :: IO Text
getLink = W.get "http://google.com"
      <&> view W.responseBody
       |> TL.decodeUtf8With T.ignore
       |> TL.toStrict

main :: IO ()
main = runTisch

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
