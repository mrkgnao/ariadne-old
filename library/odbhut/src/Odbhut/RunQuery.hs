{-# LANGUAGE FlexibleContexts #-}

module Odbhut.RunQuery (module Odbhut.RunQuery,
                         QueryRunner,
                         -- * Datatypes
                         IRQ.QueryRunnerColumn,
                         IRQ.QueryRunnerColumnDefault (..),
                         -- * Creating now 'QueryRunnerColumn's
                         IRQ.fieldQueryRunnerColumn,
                         IRQ.fieldParserQueryRunnerColumn) where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.FromRow as FR
import qualified Data.String as String

import           Odbhut.Column (Column)
import qualified Odbhut.Sql as S
import           Odbhut.QueryArr (Query)
import           Odbhut.Internal.RunQuery (QueryRunner(QueryRunner))
import qualified Odbhut.Internal.RunQuery as IRQ
import qualified Odbhut.Internal.QueryArr as Q

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

-- * Running 'Query's

-- | @runQuery@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runQuery@.
--
-- Example type specialization:
--
-- @
-- runQuery :: Query (Column 'Odbhut.PGTypes.PGInt4', Column 'Odbhut.PGTypes.PGText') -> IO [(Int, String)]
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- runQuery :: Query (Foo (Column 'Odbhut.PGTypes.PGInt4') (Column 'Odbhut.PGTypes.PGText') (Column 'Odbhut.PGTypes.PGBool')
--          -> IO [Foo Int String Bool]
-- @
--
-- Odbhut types are converted to Haskell types based on instances of
-- the 'Odbhut.Internal.RunQuery.QueryRunnerColumnDefault' typeclass.
runQuery :: D.Default QueryRunner columns haskells
         => PGS.Connection
         -> Query columns
         -> IO [haskells]
runQuery = runQueryExplicit D.def

-- | @runQueryFold@ streams the results of a query incrementally and consumes
-- the results with a left fold.
--
-- This fold is /not/ strict. The stream consumer is responsible for
-- forcing the evaluation of its result to avoid space leaks.
runQueryFold
  :: D.Default QueryRunner columns haskells
  => PGS.Connection
  -> Query columns
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFold = runQueryFoldExplicit D.def

-- * Creating new 'QueryRunnerColumn's

-- | Use 'queryRunnerColumn' to make an instance to allow you to run queries on
--   your own datatypes.  For example:
--
-- @
-- newtype Foo = Foo Int
--
-- instance QueryRunnerColumnDefault Foo Foo where
--    queryRunnerColumnDefault =
--        queryRunnerColumn ('Odbhut.Column.unsafeCoerceColumn'
--                               :: Column Foo -> Column PGInt4)
--                          Foo
--                          queryRunnerColumnDefault
-- @
queryRunnerColumn :: (Column a' -> Column a) -> (b -> b')
                  -> IRQ.QueryRunnerColumn a b -> IRQ.QueryRunnerColumn a' b'
queryRunnerColumn colF haskellF qrc = IRQ.QueryRunnerColumn (P.lmap colF u)
                                                            (fmapFP haskellF fp)
  where IRQ.QueryRunnerColumn u fp = qrc
        fmapFP = fmap . fmap . fmap

-- * Explicit versions

runQueryExplicit :: QueryRunner columns haskells
                 -> PGS.Connection
                 -> Query columns
                 -> IO [haskells]
runQueryExplicit qr conn q = maybe (return []) (PGS.queryWith_ parser conn) sql
  where (sql, parser) = prepareQuery qr q

runQueryFoldExplicit
  :: QueryRunner columns haskells
  -> PGS.Connection
  -> Query columns
  -> b
  -> (b -> haskells -> IO b)
  -> IO b
runQueryFoldExplicit qr conn q z f = case sql of
  Nothing   -> return z
  Just sql' -> PGS.foldWith_ parser conn sql' z f
  where (sql, parser) = prepareQuery qr q

-- * Deprecated functions

-- | For internal use only.  Do not use.  Will be deprecated in
-- version 0.6.
prepareQuery :: QueryRunner columns haskells -> Query columns -> (Maybe PGS.Query, FR.RowParser haskells)
prepareQuery qr@(QueryRunner u _ _) q = (sql, parser)
  where sql :: Maybe PGS.Query
        sql = fmap String.fromString (S.showSqlForPostgresExplicit u q)
        -- FIXME: We're doing work twice here
        (b, _, _) = Q.runSimpleQueryArrStart q ()
        parser = IRQ.prepareRowParser qr b
