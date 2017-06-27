{-# LANGUAGE TypeOperators #-}
module Ariadne.Database where

import Edible (Query)

data Db

-- | A convenient type operator for queries.
type (>->) a b = a -> Query Db () b

