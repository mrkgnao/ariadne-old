{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ariadne.Ambiguous
  ( colLens
  ) where

import Edible (col)
import Edible.Internal.Table (ColLens)

import Control.Lens (Lens')
import Data.Proxy (Proxy(..))

colLens :: forall s a b. ColLens s a b b => Lens' a b
colLens = col (Proxy @s)
