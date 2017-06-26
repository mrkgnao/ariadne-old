module Odbhut.Internal.Label where

import qualified Odbhut.Internal.Tag as T
import qualified Odbhut.Internal.PrimQuery as PQ

label' :: String -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
label' l (x, q, t) = (x, PQ.Label l q, t)
