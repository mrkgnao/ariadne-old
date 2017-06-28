{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Edible.AggregateTypes where
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

import qualified Edible.Internal.Column              as IC
import qualified Edible.Internal.HaskellDB.PrimQuery as HPQ
import qualified Edible.Internal.PackMap             as PM
import qualified Edible.Internal.QueryArr            as Q

import qualified Edible.Column                       as C
import qualified Edible.Join                         as J
import qualified Edible.Order                        as Ord
import qualified Edible.PGTypes                      as T
import           Edible.QueryArr                     (Query)

{-| An 'Aggregator' takes a collection of rows of type @a@, groups
    them, and transforms each group into a single row of type @b@. This
    corresponds to aggregators using @GROUP BY@ in SQL.

    You should combine basic 'Aggregator's into 'Aggregator's on compound
    types by using the operations in "Data.Profunctor.Product".

    An 'Aggregator' corresponds closely to a 'Control.Foldl.Fold' from the
    @foldl@ package.  Whereas an 'Aggregator' @a@ @b@ takes each group of
    type @a@ to a single row of type @b@, a 'Control.Foldl.Fold' @a@ @b@
    takes a list of @a@ and returns a single row of type @b@.
-}
newtype Aggregator a b =
  Aggregator (PM.PackMap (Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct), HPQ.PrimExpr) HPQ.PrimExpr a b)

{-| Given a 'Query' producing rows of type @a@ and an 'Aggregator' accepting rows of
    type @a@, apply the aggregator to the results of the query.

    If you simply want to count the number of rows in a query you might
    find the 'countRows' function more convenient.

    By design there is no aggregation function of type @Aggregator b b' ->
    QueryArr a b -> QueryArr a b'@.  Such a function would allow violation
    of SQL's scoping rules and lead to invalid queries.

    Please note that when aggregating an empty query with no @GROUP BY@
    clause, Edible's behaviour differs from Postgres's behaviour.
    Postgres returns a single row whereas Edible returns zero rows.
    Edible's behaviour is consistent with the meaning of aggregating
    over groups of rows and Postgres's behaviour is inconsistent.  When a
    query has zero rows it has zero groups, and thus zero rows in the
    result of an aggregation.
-}
aggregate :: Aggregator a b -> Query a -> Query b
aggregate agg q = Q.simpleQueryArr (aggregateU agg . Q.runSimpleQueryArr q)

-- | Group the aggregation by equality on the input to 'groupBy'.
groupBy :: Aggregator (C.Column a) (C.Column a)
groupBy = makeAggr' Nothing

makeAggr' :: Maybe HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr' m =
  Aggregator
    (PM.PackMap
       (\f (C.Column e) -> fmap C.Column (f (fmap (, [], HPQ.AggrAll) m, e))))

aggregateU :: Aggregator a b
           -> (a, PQ.PrimQuery, T.Tag) -> (b, PQ.PrimQuery, T.Tag)
aggregateU agg (c0, primQ, t0) = (c1, primQ', T.next t0)
  where (c1, projPEs) =
          PM.run (runAggregator agg (extractAggregateFields t0) c0)

        primQ' = PQ.Aggregate projPEs primQ

extractAggregateFields :: T.Tag -> (Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct), HPQ.PrimExpr)
      -> PM.PM [(HPQ.Symbol, (Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct), HPQ.PrimExpr))] HPQ.PrimExpr
extractAggregateFields = PM.extractAttr "result"


runAggregator :: Applicative f => Aggregator a b
              -> ((Maybe (HPQ.AggrOp, [HPQ.OrderExpr], HPQ.AggrDistinct), HPQ.PrimExpr) -> f HPQ.PrimExpr)
              -> a -> f b
runAggregator (Aggregator a) = PM.traversePM a

-- { Boilerplate instances

instance Functor (Aggregator a) where
  fmap f (Aggregator g) = Aggregator (fmap f g)

instance Applicative (Aggregator a) where
  pure = Aggregator . pure
  Aggregator f <*> Aggregator x = Aggregator (f <*> x)

instance P.Profunctor Aggregator where
  dimap f g (Aggregator q) = Aggregator (P.dimap f g q)

instance PP.ProductProfunctor Aggregator where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor Aggregator where
  Aggregator x1 +++! Aggregator x2 = Aggregator (x1 PP.+++! x2)

-- }
