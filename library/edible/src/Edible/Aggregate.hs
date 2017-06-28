{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Perform aggregation on 'Query's.  To aggregate a 'Query' you
-- should construct an 'Aggregator' encoding how you want the
-- aggregation to proceed, then call 'aggregate' on it.  The
-- 'Aggregator' should be constructed from the basic 'Aggregator's
-- below by using the combining operations from
-- "Data.Profunctor.Product".
module Edible.Aggregate
  ( Aggregator
  , orderAggregator
  , aggregate
  , groupBy
  , AggSum
  , sumgg
  , countgg
  , countngg
  , countRows
  , AggAvg
  , avggg
  , bwandgg
  , bworgg
  , landgg
  , lorgg
  , maxgg
  , mingg
  , arraygg
  , arrayngg
  , jsonarraygg
  , jsonbarraygg
  , textgg
  , byteagg
  , AggStdDev
  , stddevgg
  , stddevpopgg
  , variancegg
  , variancepopgg
  -- * Aggregation
  , aggregate
  , Aggregator
  -- * Basic 'Aggregator's
  , groupBy
  , Edible.Aggregate.sum
  , count
  , countStar
  , avg
  , Edible.Aggregate.max
  , Edible.Aggregate.min
  , boolOr
  , boolAnd
  , arrayAgg
  , stringAgg
  -- * Counting rows
  , countRows
  -- * Entire module
  ) where

import qualified Data.Profunctor                     as P
import qualified Edible.Internal.Column              as OI
import qualified Edible.Internal.HaskellDB.PrimQuery as HDB
import qualified Edible.Operators                    as OI
import qualified Edible.Order                        as E
import qualified Edible.PGTypes                      as E
import           GHC.TypeLits                        (type (+), CmpNat,
                                                      KnownNat)


import           Edible.Internal.Kol                 (Kol (..), PGArrayn,
                                                      PgTyped (..))
import           Edible.Internal.Koln                (Koln (..))
import           Edible.Internal.QueryTypes          (Query (..))
import           Edible.Operators                    (PgEq, PgIntegral, PgNum,
                                                      PgOrd)

import           Control.Applicative                 (Applicative, pure, (<*>))

import qualified Data.Profunctor                     as P
import qualified Data.Profunctor.Product             as PP

import qualified Edible.Internal.Column              as C
import qualified Edible.Internal.Order               as O
import qualified Edible.Internal.PackMap             as PM
import qualified Edible.Internal.PrimQuery           as PQ
import qualified Edible.Internal.Tag                 as T

import qualified Edible.Internal.HaskellDB.PrimQuery as HPQ

import           Control.Applicative                 (pure)
import           Data.Profunctor                     (lmap)
import qualified Data.Profunctor                     as P

import  Edible.AggregateTypes               (Aggregator(..))
import qualified Edible.AggregateTypes               as E
import qualified Edible.Internal.Column              as IC
import qualified Edible.Internal.HaskellDB.PrimQuery as HPQ
import qualified Edible.Internal.PackMap             as PM
import qualified Edible.Internal.QueryArr            as Q

import qualified Edible.Column                       as C
import qualified Edible.Join                         as J
import qualified Edible.Order                        as Ord
import qualified Edible.PGTypes                      as T
import qualified Edible.QueryArr                     as Q (Query)

-- This page of Postgres documentation tell us what aggregate
-- functions are available
--
--   http://www.postgresql.org/docs/9.3/static/functions-aggregate.html

-- | Order the values within each aggregation in `Aggregator` using
-- the given ordering. This is only relevant for aggregations that
-- depend on the order they get their elements, like `arrayAgg` and
-- `stringAgg`.
--
-- Note that this orders all aggregations with the same ordering. If
-- you need different orderings for different aggregations, use
-- 'Edible.Internal.Aggregate.orderAggregate'.

aggregateOrdered  :: Ord.Order a -> Aggregator a b -> Q.Query a -> Q.Query b
aggregateOrdered o agg = E.aggregate (orderAggregate o agg)

-- | Aggregate only distinct values
distinctAggregator :: Aggregator a b -> Aggregator a b
distinctAggregator (Aggregator (PM.PackMap pm)) =
  Aggregator (PM.PackMap (\f c -> pm (f . P.first' (fmap (\(a,b,_) -> (a,b,HPQ.AggrDistinct)))) c))

-- | Sum all rows in a group.
sum :: Aggregator (C.Column a) (C.Column a)
sum = makeAggr HPQ.AggrSum

-- | Count the number of non-null rows in a group.
count :: Aggregator (C.Column a) (C.Column T.PGInt8)
count = makeAggr HPQ.AggrCount

-- | Count the number of rows in a group.  This 'Aggregator' is named
-- @countStar@ after SQL's @COUNT(*)@ aggregation function.
countStar :: Aggregator a (C.Column T.PGInt8)
countStar = lmap (const (0 :: C.Column T.PGInt4)) count

-- | Average of a group
avg :: Aggregator (C.Column T.PGFloat8) (C.Column T.PGFloat8)
avg = makeAggr HPQ.AggrAvg

-- | Maximum of a group
max :: Ord.PGOrd a => Aggregator (C.Column a) (C.Column a)
max = makeAggr HPQ.AggrMax

-- | Maximum of a group
min :: Ord.PGOrd a => Aggregator (C.Column a) (C.Column a)
min = makeAggr HPQ.AggrMin

boolOr :: Aggregator (C.Column T.PGBool) (C.Column T.PGBool)
boolOr = makeAggr HPQ.AggrBoolOr

boolAnd :: Aggregator (C.Column T.PGBool) (C.Column T.PGBool)
boolAnd = makeAggr HPQ.AggrBoolAnd

arrayAgg :: Aggregator (C.Column a) (C.Column (T.PGArray a))
arrayAgg = makeAggr HPQ.AggrArr

stringAgg :: C.Column T.PGText -> Aggregator (C.Column T.PGText) (C.Column T.PGText)
stringAgg = E.makeAggr' . Just . HPQ.AggrStringAggr . IC.unColumn

-- | Count the number of rows in a query.  This is different from
-- 'aggregate' 'count' because it always returns exactly one row, even
-- when the input query is empty.

-- This is currently implemented in a cheeky way with a LEFT JOIN.  If
-- there are any performance issues it could be rewritten to use an
-- SQL COUNT aggregation which groups by nothing.  This would require
-- changing the AST though, so I'm not too keen.
--
-- See https://github.com/tomjaguarpaw/haskell-opaleye/issues/162
countRows' :: Q.Query a -> Q.Query (C.Column T.PGInt8)
countRows' = fmap (C.fromNullable 0)
            . fmap snd
            . (\q -> J.leftJoin (pure ())
                                (E.aggregate count q)
                                (const (T.pgBool True)))
            . fmap (const (0 :: C.Column T.PGInt4))
            --- ^^ The count aggregator requires an input of type
            -- 'Column a' rather than 'a' (I'm not sure if there's a
            -- good reason for this).  To deal with that restriction
            -- we just map a dummy integer value over it.

makeAggr :: HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr = E.makeAggr' . Just

-- | Order the values within each aggregation in `Aggregator` using
-- the given ordering. This is only relevant for aggregations that
-- depend on the order they get their elements, like
-- `Edible.Aggregate.arrayAgg` and `Edible.Aggregate.stringAgg`.
--
-- You can either apply it to an aggregation of multiple columns, in
-- which case it will apply to all aggregation functions in there, or you
-- can apply it to a single column, and then compose the aggregations
-- afterwards. Examples:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray a))
-- > x = (,) <$> orderAggregate (asc snd) (lmap fst arrayAggGrouped)
-- >         <*> orderAggregate (desc snd) (lmap fst arrayAggGrouped)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(a ORDER BY b DESC)
-- FROM (SELECT a, b FROM ...)
-- @
--
-- Or:
--
-- > x :: Aggregator (Column a, Column b) (Column (PGArray a), Column (PGArray b))
-- > x = orderAggregate (asc snd) $ p2 (arrayAggGrouped, arrayAggGrouped)
--
-- This will generate:
--
-- @
-- SELECT array_agg(a ORDER BY b ASC), array_agg(b ORDER BY b ASC)
-- FROM (SELECT a, b FROM ...)
-- @

orderAggregate :: O.Order a -> Aggregator a b -> Aggregator a b
orderAggregate o (Aggregator (PM.PackMap pm)) =
  Aggregator (PM.PackMap (\f c -> pm (f . P.first' (fmap ((\f' (a,b,c') -> (a,f' b,c')) (const $ O.orderExprs c o)))) c))

--------------------------------------------------------------------------------

-- | Order the values within each aggregation in `Aggregator` using the given
-- ordering. This is only relevant for aggregations that depend on the order
-- they get their elements, like 'arraygg' or 'textgg'.
--
-- You can either apply it to an aggregation of multiple columns, in which case
-- it will apply to all aggregation functions in there, or you can apply it to a
-- single column, and then compose the aggregations afterwards. Examples:
--
-- @
-- x :: 'Aggregator' ('Kol' a, 'Kol' b) ('Kol' ('E.PGArray' a), 'Kol' ('E.PGArray' a))
-- x = (,) <$> 'orderAggregator' ('Edible.asc' 'snd')  ('P.lmap' 'fst' 'arraygg')
--         <*> 'orderAggregator' ('Edible.descl 'snd') ('P.lmap' 'fst' 'arraygg')
-- @
--
-- This will generate:
--
--
-- > SELECT array_agg(a ORDER BY b ASC),
-- >        array_agg(a ORDER BY b DESC)
-- > FROM (SELECT a, b FROM ...)
--
--
-- Or:
--
-- @
-- x :: 'Aggregator' ('Kol' a, 'Kol' b) ('Kol' ('E.PGArray' a), 'Kol' ('E.PGArray' a))
-- x = 'orderAggregator' ('Edible.asc' 'snd') $ 'Edible.Internal.Profunctors.ppa' ('arraygg', 'arraygg')
-- @
--
-- This will generate:
--
-- > SELECT array_agg(a ORDER BY b ASC),
-- >        array_agg(b ORDER BY b ASC)
-- > FROM (SELECT a, b FROM ...)
orderAggregator :: PgOrd a => E.Order a -> Aggregator a b -> Aggregator a b
orderAggregator = orderAggregate

--------------------------------------------------------------------------------

{-| Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting
    rows of type @a@, apply the aggregator to the results of the query.

    Please note that when aggregating an empty query with no @GROUP BY@ clause,
    @tisch@'s behaviour differs from Postgres's behaviour. PostgreSQL
    returns a single row whereas @tisch@ returns zero rows.
    @tisch@'s behaviour is consistent with the meaning of aggregating over
    groups of rows, unlike PostgreSQL's behaviour: when a query has zero rows
    it has zero groups, and thus zero rows in the result of an aggregation.

    If you simply want to count the number of rows in a query you might find the
    'countRows' function more convenient.

    By design there is no aggregation function of type @Aggregator a b ->
    Query d x a -> QueryArr d x b@, as such a function would allow violation of
    SQL's scoping rules and lead to invalid queries.
-}
aggregate :: Aggregator a b -> Query d () a -> Query d () b
aggregate f = Query . E.aggregate f . unQuery

--------------------------------------------------------------------------------

-- | Group the aggregation by equality.
groupBy :: PgEq a => Aggregator (Kol a) (Kol a)
groupBy = P.dimap unKol Kol E.groupBy

-- | Instances of 'AggSum' can be used with 'sumgg'.
--
-- TODO: Support all possible inputs and ouputs. See PostgreSQL docs.
class (PgNum a, PgNum b) => AggSum a b
instance {-# OVERLAPPABLE #-} PgNum a => AggSum a a
instance AggSum E.PGInt2 E.PGInt8
instance AggSum E.PGInt4 E.PGInt8
instance KnownNat s => AggSum E.PGInt8 (E.PGNumeric s)
instance AggSum E.PGFloat4 E.PGFloat8
instance AggSum E.PGInt8 (E.PGNumeric 0)

-- | Add the values in input columns.
sumgg :: AggSum a b => Aggregator (Kol a) (Kol b)
sumgg = unsafeMakeAggr HDB.AggrSum

-- | Count the number of non-@NULL@ input values.
--
-- See also: 'countRows', 'countngg'.
countgg :: Aggregator (Kol a) (Kol E.PGInt8)
countgg = P.dimap unKol Kol count

-- | Count the number of input values, whether they are @NULL@ or not.
--
-- See also: 'countRows', 'countgg'.
countngg :: Aggregator (Koln a) (Kol E.PGInt8)
countngg = P.rmap Kol countStar

-- | Count the number of rows in a 'Query'.
--
-- This is different from @'aggregate' 'countngg'@ because this always
-- returns exactly one row, even when the input 'Query' is empty.
--
-- See also: 'countgg', 'countngg'.
countRows :: Query d () a -> Query d () (Kol E.PGInt8)
countRows = Query . fmap Kol . countRows' . unQuery

-- | Instances of 'AggAvg' can be used with 'aggAvg'.
--
-- TODO: Support all possible inputs and ouputs. See PostgreSQL docs.
class (PgNum a, PgNum b) => AggAvg a b
instance {-# OVERLAPPABLE #-} PgNum a => AggAvg a a
instance AggAvg E.PGFloat4 E.PGFloat8
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggAvg E.PGInt2 (E.PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggAvg E.PGInt4 (E.PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggAvg E.PGInt8 (E.PGNumeric s)
-- | Warning: Depending on your choice of @s'@, you might be getting less
-- resolution than expected.
instance (KnownNat s, KnownNat s', CmpNat s (s' + 1) ~ 'GT) => AggAvg (E.PGNumeric s) (E.PGNumeric s')

-- | The average (arithmetic mean) of all input values
avggg :: AggAvg a b => Aggregator (Kol a) (Kol b)
avggg = unsafeMakeAggr HDB.AggrAvg

-- | Bitwise AND of all input values.
bwandgg :: PgIntegral a => Aggregator (Kol a) (Kol a)
bwandgg = unsafeMakeAggr (HDB.AggrOther "bit_and")

-- | Bitwise OR of all input values.
bworgg :: PgIntegral a => Aggregator (Kol a) (Kol a)
bworgg = unsafeMakeAggr (HDB.AggrOther "bit_or")

-- | Logical AND of all input values.
landgg :: Aggregator (Kol E.PGBool) (Kol E.PGBool)
landgg = unsafeMakeAggr HDB.AggrBoolAnd

-- | Logical OR of all input values.
lorgg :: Aggregator (Kol E.PGBool) (Kol E.PGBool)
lorgg = unsafeMakeAggr HDB.AggrBoolOr

-- | Maximum value of all input values.
maxgg :: PgOrd a => Aggregator (Kol a) (Kol a)
maxgg = unsafeMakeAggr HDB.AggrMax

-- | Minimum value of all input values.
mingg :: PgOrd a => Aggregator (Kol a) (Kol a)
mingg = unsafeMakeAggr HDB.AggrMin

-- | Collect all non-@NULL@ input values into a 'E.PGArray'.
arraygg :: PgTyped a => Aggregator (Kol a) (Kol (E.PGArray a))
arraygg = unsafeMakeAggr HDB.AggrArr

-- | Collect all nullable input values into a 'E.PGArrayn'.
arrayngg :: PgTyped a => Aggregator (Koln a) (Kol (PGArrayn a))
arrayngg = P.dimap unKoln Kol (makeAggr HDB.AggrArr)

-- | Aggregates values as a 'E.PGJson' array.
jsonarraygg :: Aggregator (Kol a) (Kol E.PGJson)
jsonarraygg = unsafeMakeAggr (HDB.AggrOther "json_agg")

-- | Aggregates values as a 'E.PGJsonb' array.
jsonbarraygg :: Aggregator (Kol a) (Kol E.PGJsonb)
jsonbarraygg = unsafeMakeAggr (HDB.AggrOther "jsonb_agg")

-- | Aggregates 'E.PGText' values by concatenating them using the given
-- separator.
textgg :: Kol E.PGText -> Aggregator (Kol E.PGText) (Kol E.PGText)
textgg = unsafeMakeAggr . HDB.AggrStringAggr . OI.unColumn . unKol

-- | Aggregates 'E.PGBytea' values by concatenating them using the given
-- separator.
byteagg :: Kol E.PGBytea -> Aggregator (Kol E.PGBytea) (Kol E.PGBytea)
byteagg = unsafeMakeAggr . HDB.AggrStringAggr . OI.unColumn . unKol

-- | Instances of 'AggStdDev' can be used with 'stddevgg',
-- 'stddevpopgg', 'variance' and 'variancepopgg'.
class (PgNum a, PgNum b) => AggStdDev a b
instance AggStdDev E.PGFloat4 E.PGFloat8
instance AggStdDev E.PGFloat8 E.PGFloat8
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggStdDev E.PGInt2 (E.PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggStdDev E.PGInt4 (E.PGNumeric s)
-- | Warning: Depending on your choice of @s@, you might be getting less
-- resolution than expected.
instance KnownNat s => AggStdDev E.PGInt8 (E.PGNumeric s)
-- | Warning: Depending on your choice of @s'@, you might be getting less
-- resolution than expected.
instance (KnownNat s, KnownNat s', CmpNat s (s' + 1) ~ 'GT) => AggStdDev (E.PGNumeric s) (E.PGNumeric s')

-- | Sample standard deviation of the input values.
stddevgg :: AggStdDev a b => Aggregator (Kol a) (Kol b)
stddevgg = unsafeMakeAggr HDB.AggrStdDev

-- | Population standard deviation of the input values.
stddevpopgg :: AggStdDev a b => Aggregator (Kol a) (Kol b)
stddevpopgg = unsafeMakeAggr HDB.AggrStdDevP

-- | Sample variance of the input values (square of the sample standard
-- deviation 'stdevgg').
variancegg :: AggStdDev a b => Aggregator (Kol a) (Kol b)
variancegg = unsafeMakeAggr HDB.AggrVar

-- | Population variance of the input values (square of the population standard
-- deviation 'stdevoppgg').
variancepopgg :: AggStdDev a b => Aggregator (Kol a) (Kol b)
variancepopgg = unsafeMakeAggr HDB.AggrVarP

--------------------------------------------------------------------------------

unsafeMakeAggr :: PgTyped b => HDB.AggrOp -> Aggregator (Kol a) (Kol b)
unsafeMakeAggr x = P.dimap unKol Kol (makeAggr x)
{-# INLINE unsafeMakeAggr #-}
