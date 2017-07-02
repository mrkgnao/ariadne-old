{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

module Edible.RunQuery (module Edible.RunQuery,
                         QueryRunner,
                         -- * Datatypes
                         IRQ.QueryRunnerColumn,
                         IRQ.QueryRunnerColumnDefault (..),
                         -- * Creating now 'QueryRunnerColumn's
                         IRQ.fieldQueryRunnerColumn,
                         IRQ.fieldParserQueryRunnerColumn,
                         unsafeUnNullableColumn,
                         unsafeFunExpr
                         ) where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.FromRow as FR
import qualified Data.String as String

import           Edible.Column (Column)
import qualified Edible.Sql as S
import           Edible.QueryArr (Query)
import           Edible.Internal.RunQuery (QueryRunner(QueryRunner))
import qualified Edible.Internal.RunQuery as IRQ
import qualified Edible.Internal.QueryArr as Q

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product.Default as D

import qualified Edible.Column                        as O



import qualified Edible.Internal.Column               as OI
import qualified Edible.Internal.HaskellDB.PrimQuery  as HDB



-- * Running 'Query's

-- | @runQuery@'s use of the 'D.Default' typeclass means that the
-- compiler will have trouble inferring types.  It is strongly
-- recommended that you provide full type signatures when using
-- @runQuery@.
--
-- Example type specialization:
--
-- @
-- runQuery :: Query (Column 'Edible.PGTypes.PGInt4', Column 'Edible.PGTypes.PGText') -> IO [(Int, String)]
-- @
--
-- Assuming the @makeAdaptorAndInstance@ splice has been run for the product type @Foo@:
--
-- @
-- runQuery :: Query (Foo (Column 'Edible.PGTypes.PGInt4') (Column 'Edible.PGTypes.PGText') (Column 'Edible.PGTypes.PGBool')
--          -> IO [Foo Int String Bool]
-- @
--
-- Edible types are converted to Haskell types based on instances of
-- the 'Edible.Internal.RunQuery.QueryRunnerColumnDefault' typeclass.
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
--        queryRunnerColumn ('Edible.Column.unsafeCoerceColumn'
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
runQueryExplicit qr conn q = maybe (pure []) (PGS.queryWith_ parser conn) sql
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


data SomeColumn = forall a. SomeColumn (O.Column a)

-- | 'unsafeFunExpr "f" xs' calls a function called @"f"@ with arguments @xs@.
-- The return type must correctly be set by the caller.
unsafeFunExpr :: HDB.Name -> [SomeColumn] -> O.Column b
unsafeFunExpr fname =
  OI.Column . HDB.FunExpr fname . map (\(SomeColumn (OI.Column x)) -> x)

unsafeUnNullableColumn :: O.Column (O.Nullable a) -> O.Column a
unsafeUnNullableColumn = O.unsafeCoerceColumn
