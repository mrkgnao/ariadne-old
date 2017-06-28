{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This is an internal module. You are very discouraged from using it directly.
--
-- This module exports stuff that doesn't really fit into the other internal
-- modules.
module Edible.Internal.QueryTypes
 ( Query(..)
 ) where

import           Control.Arrow           (Arrow)
import           Control.Category        (Category)
import           Data.Profunctor         (Profunctor)
import           Data.Profunctor.Product (ProductProfunctor)

import qualified Edible.QueryArr         as O


--------------------------------------------------------------------------------

-- | A wrapper around @opaleye@'s 'O.QuerryArr' adding a placeholder @t@, which
-- shall mention the 'Database' associated with the query.
--
-- Note that, contrary to @opaleye@, we don't make a distinction between
-- 'O.QueryArr' and 'O.Query', as we think that hurts comprehension. We always
-- use our 'Query' type instead, which behaves as @opaleye@'s 'O.QueryArr', not
-- as @opaleye@'s 'O.Query'.
newtype Query (d :: k) a b = Query { unQuery :: O.QueryArr a b }
 deriving (Functor, Applicative, Category, Arrow, Profunctor, ProductProfunctor)
