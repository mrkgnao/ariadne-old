module Edible.Internal.Label where

import qualified Edible.Internal.Tag as T
import qualified Edible.Internal.PrimQuery as PQ

label' :: String -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
label' l (x, q, t) = (x, PQ.Label l q, t)
