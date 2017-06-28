{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Edible.Internal.Debug
 ( renderSqlQuery
 , renderSqlQuery'
 ) where

import qualified Data.Profunctor.Product.Default as PP
import qualified Edible.Internal.Unpackspec      as OI
import qualified Edible.Sql                      as O

import           Edible.Internal.Query            (Query (..))

--------------------------------------------------------------------------------

renderSqlQuery
  :: forall d v. (PP.Default OI.Unpackspec v v) => Query d () v -> Maybe String
renderSqlQuery = renderSqlQuery' (PP.def :: OI.Unpackspec v v)

renderSqlQuery' :: OI.Unpackspec v v' -> Query d () v -> Maybe String
renderSqlQuery' u = O.showSqlForPostgresExplicit u . unQuery




