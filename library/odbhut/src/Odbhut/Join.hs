-- | Left, right, and full outer joins.
--
-- "Odbhut.FunctionalJoin" provides a much nicer, Haskelly, interface
-- to joins than this module, which sticks to the (horrible) standard
-- \"make missing rows NULL\" interface that SQL provides.
--
-- If you want inner joins, just use 'restrict' instead.
--
-- The use of the 'D.Default' typeclass means that the compiler will
-- have trouble inferring types.  It is strongly recommended that you
-- provide full type signatures when using the join functions.
--
-- Example specialization:
--
-- @
-- leftJoin :: Query (Column a, Column b)
--          -> Query (Column c, Column (Nullable d))
--          -> (((Column a, Column b), (Column c, Column (Nullable d))) -> Column 'Odbhut.PGTypes.PGBool')
--          -> Query ((Column a, Column b), (Column (Nullable c), Column (Nullable d)))
-- @

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Odbhut.Join where

import qualified Odbhut.Internal.Unpackspec as U
import qualified Odbhut.Internal.Join as J
import qualified Odbhut.Internal.PrimQuery as PQ
import           Odbhut.QueryArr (Query)
import           Odbhut.Internal.Column (Column)
import qualified Odbhut.PGTypes as T

import qualified Data.Profunctor.Product.Default as D

-- * Joins

leftJoin  :: (D.Default U.Unpackspec columnsL columnsL,
              D.Default U.Unpackspec columnsR columnsR,
              D.Default J.NullMaker columnsR nullableColumnsR)
          => Query columnsL  -- ^ Left query
          -> Query columnsR  -- ^ Right query
          -> ((columnsL, columnsR) -> Column T.PGBool) -- ^ Condition on which to join
          -> Query (columnsL, nullableColumnsR) -- ^ Left join
leftJoin = leftJoinExplicit D.def D.def D.def

rightJoin  :: (D.Default U.Unpackspec columnsL columnsL,
               D.Default U.Unpackspec columnsR columnsR,
               D.Default J.NullMaker columnsL nullableColumnsL)
           => Query columnsL -- ^ Left query
           -> Query columnsR -- ^ Right query
           -> ((columnsL, columnsR) -> Column T.PGBool) -- ^ Condition on which to join
           -> Query (nullableColumnsL, columnsR) -- ^ Right join
rightJoin = rightJoinExplicit D.def D.def D.def


fullJoin  :: (D.Default U.Unpackspec columnsL columnsL,
              D.Default U.Unpackspec columnsR columnsR,
              D.Default J.NullMaker columnsL nullableColumnsL,
              D.Default J.NullMaker columnsR nullableColumnsR)
          => Query columnsL -- ^ Left query
          -> Query columnsR -- ^ Right query
          -> ((columnsL, columnsR) -> Column T.PGBool) -- ^ Condition on which to join
          -> Query (nullableColumnsL, nullableColumnsR) -- ^ Full outer join
fullJoin = fullJoinExplicit D.def D.def D.def D.def

-- * Explicit versions

leftJoinExplicit :: U.Unpackspec columnsL columnsL
                 -> U.Unpackspec columnsR columnsR
                 -> J.NullMaker columnsR nullableColumnsR
                 -> Query columnsL -> Query columnsR
                 -> ((columnsL, columnsR) -> Column T.PGBool)
                 -> Query (columnsL, nullableColumnsR)
leftJoinExplicit uA uB nullmaker =
  J.joinExplicit uA uB id (J.toNullable nullmaker) PQ.LeftJoin

rightJoinExplicit :: U.Unpackspec columnsL columnsL
                  -> U.Unpackspec columnsR columnsR
                  -> J.NullMaker columnsL nullableColumnsL
                  -> Query columnsL -> Query columnsR
                  -> ((columnsL, columnsR) -> Column T.PGBool)
                  -> Query (nullableColumnsL, columnsR)
rightJoinExplicit uA uB nullmaker =
  J.joinExplicit uA uB (J.toNullable nullmaker) id PQ.RightJoin


fullJoinExplicit :: U.Unpackspec columnsL columnsL
                 -> U.Unpackspec columnsR columnsR
                 -> J.NullMaker columnsL nullableColumnsL
                 -> J.NullMaker columnsR nullableColumnsR
                 -> Query columnsL -> Query columnsR
                 -> ((columnsL, columnsR) -> Column T.PGBool)
                 -> Query (nullableColumnsL, nullableColumnsR)
fullJoinExplicit uA uB nullmakerA nullmakerB =
  J.joinExplicit uA uB (J.toNullable nullmakerA) (J.toNullable nullmakerB) PQ.FullJoin
