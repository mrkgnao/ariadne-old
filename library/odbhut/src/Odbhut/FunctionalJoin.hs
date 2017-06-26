-- | Left, right, and full outer joins.
--
-- The interface in this module is much nicer than the standard \"make
-- missing rows NULL\" interface that SQL provides.  If you really
-- want the standard interface then use "Odbhut.Join".

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Odbhut.FunctionalJoin where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   ((<<<))

import qualified Data.Profunctor.Product.Default as D
import qualified Data.Profunctor.Product         as PP

import qualified Odbhut.Column                  as C
import           Odbhut.Internal.Column         (Column, Nullable)
import qualified Odbhut.Internal.Join           as IJ
import qualified Odbhut.Internal.Operators      as IO
import qualified Odbhut.Internal.Unpackspec     as IU
import qualified Odbhut.Join                    as J
import qualified Odbhut.PGTypes                 as T
import qualified Odbhut.Operators               as O
import           Odbhut.QueryArr                (Query)

joinF :: (columnsL -> columnsR -> columnsResult)
      -- ^ Calculate result columns from input columns
      -> (columnsL -> columnsR -> Column T.PGBool)
      -- ^ Condition on which to join
      -> Query columnsL
      -- ^ Left query
      -> Query columnsR
      -- ^ Right query
      -> Query columnsResult
joinF f cond l r =
  fmap (uncurry f) (O.keepWhen (uncurry cond) <<< ((,) <$> l <*> r))

leftJoinF :: (D.Default IO.IfPP columnsResult columnsResult,
              D.Default IU.Unpackspec columnsL columnsL,
              D.Default IU.Unpackspec columnsR columnsR)
          => (columnsL -> columnsR -> columnsResult)
          -- ^ Calculate result row from input rows for rows in the
          -- right query satisfying the join condition
          -> (columnsL -> columnsResult)
          -- ^ Calculate result row from input row when there are /no/
          -- rows in the right query satisfying the join condition
          -> (columnsL -> columnsR -> Column T.PGBool)
          -- ^ Condition on which to join
          -> Query columnsL
          -- ^ Left query
          -> Query columnsR
          -- ^ Right query
          -> Query columnsResult
leftJoinF f fL cond l r = fmap ret j
  where a1 = fmap (\x -> (x, T.pgBool True))
        j  = J.leftJoinExplicit D.def
                                D.def
                                (PP.p2 ((IJ.NullMaker id), nullmakerBool))
                                l
                                (a1 r)
                                (\(l', (r', _)) -> cond l' r')

        ret (lr, (rr, rc)) = O.ifThenElseMany (C.isNull rc) (fL lr) (f lr rr)

        nullmakerBool :: IJ.NullMaker (Column T.PGBool)
                                      (Column (Nullable T.PGBool))
        nullmakerBool = D.def

rightJoinF :: (D.Default IO.IfPP columnsResult columnsResult,
               D.Default IU.Unpackspec columnsL columnsL,
               D.Default IU.Unpackspec columnsR columnsR)
           => (columnsL -> columnsR -> columnsResult)
           -- ^ Calculate result row from input rows for rows in the
           -- left query satisfying the join condition
           -> (columnsR -> columnsResult)
           -- ^ Calculate result row from input row when there are /no/
           -- rows in the left query satisfying the join condition
           -> (columnsL -> columnsR -> Column T.PGBool)
           -- ^ Condition on which to join
           -> Query columnsL
           -- ^ Left query
           -> Query columnsR
           -- ^ Right query
           -> Query columnsResult
rightJoinF f fR cond l r = fmap ret j
  where a1 = fmap (\x -> (x, T.pgBool True))
        j  = J.rightJoinExplicit D.def
                                 D.def
                                 (PP.p2 ((IJ.NullMaker id), nullmakerBool))
                                 (a1 l)
                                 r
                                 (\((l', _), r') -> cond l' r')

        ret ((lr, lc), rr) = O.ifThenElseMany (C.isNull lc) (fR rr) (f lr rr)

        nullmakerBool :: IJ.NullMaker (Column T.PGBool)
                                      (Column (Nullable T.PGBool))
        nullmakerBool = D.def

fullJoinF :: (D.Default IO.IfPP columnsResult columnsResult,
              D.Default IU.Unpackspec columnsL columnsL,
              D.Default IU.Unpackspec columnsR columnsR)
          => (columnsL -> columnsR -> columnsResult)
           -- ^ Calculate result row from input rows for rows in the
           -- left and right query satisfying the join condition
          -> (columnsL -> columnsResult)
           -- ^ Calculate result row from left input row when there
           -- are /no/ rows in the right query satisfying the join
           -- condition
          -> (columnsR -> columnsResult)
           -- ^ Calculate result row from right input row when there
           -- are /no/ rows in the left query satisfying the join
           -- condition
          -> (columnsL -> columnsR -> Column T.PGBool)
          -- ^ Condition on which to join
          -> Query columnsL
          -- ^ Left query
          -> Query columnsR
          -- ^ Right query
          -> Query columnsResult
fullJoinF f fL fR cond l r = fmap ret j
  where a1 = fmap (\x -> (x, T.pgBool True))
        j  = J.fullJoinExplicit D.def
                                D.def
                                (PP.p2 ((IJ.NullMaker id), nullmakerBool))
                                (PP.p2 ((IJ.NullMaker id), nullmakerBool))
                                (a1 l)
                                (a1 r)
                                (\((l', _), (r', _)) -> cond l' r')

        ret ((lr, lc), (rr, rc)) = O.ifThenElseMany (C.isNull lc)
                                     (fR rr)
                                     (O.ifThenElseMany (C.isNull rc)
                                        (fL lr)
                                        (f lr rr))

        nullmakerBool :: IJ.NullMaker (Column T.PGBool)
                                      (Column (Nullable T.PGBool))
        nullmakerBool = D.def
