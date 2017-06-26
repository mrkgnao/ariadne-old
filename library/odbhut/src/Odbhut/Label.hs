module Odbhut.Label where

import           Odbhut.QueryArr (Query)
import qualified Odbhut.Internal.Label as L
import qualified Odbhut.Internal.QueryArr as Q

-- | Add a commented label to the generated SQL.
label :: String -> Query a -> Query a
label l a = Q.simpleQueryArr (L.label' l . Q.runSimpleQueryArr a)
