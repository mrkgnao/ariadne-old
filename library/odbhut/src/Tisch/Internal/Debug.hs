{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tisch.Internal.Debug
 ( renderSqlQuery
 , renderSqlQuery'
 ) where

import qualified Data.Profunctor.Product.Default as PP
import qualified Odbhut.Internal.Unpackspec      as OI
import qualified Odbhut.Sql                      as O

import           Tisch.Internal.Query            (Query (..))

--------------------------------------------------------------------------------

renderSqlQuery
  :: forall d v. (PP.Default OI.Unpackspec v v) => Query d () v -> Maybe String
renderSqlQuery = renderSqlQuery' (PP.def :: OI.Unpackspec v v)

renderSqlQuery' :: OI.Unpackspec v v' -> Query d () v -> Maybe String
renderSqlQuery' u = O.showSqlForPostgresExplicit u . unQuery




