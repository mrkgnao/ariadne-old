module Edible.Label where

import           Edible.QueryArr (Query)
import qualified Edible.Internal.Label as L
import qualified Edible.Internal.QueryArr as Q

-- | Add a commented label to the generated SQL.
label :: String -> Query a -> Query a
label l a = Q.simpleQueryArr (L.label' l . Q.runSimpleQueryArr a)
