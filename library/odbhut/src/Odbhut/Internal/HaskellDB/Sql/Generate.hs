-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style

module Odbhut.Internal.HaskellDB.Sql.Generate (SqlGenerator(..)) where

import Odbhut.Internal.HaskellDB.PrimQuery
import Odbhut.Internal.HaskellDB.Sql

import qualified Data.List.NonEmpty as NEL

data SqlGenerator = SqlGenerator
    {
     sqlUpdate      :: SqlTable -> [PrimExpr] -> Assoc -> SqlUpdate,
     sqlDelete      :: SqlTable -> [PrimExpr] -> SqlDelete,
     sqlInsert      :: SqlTable -> [Attribute] -> NEL.NonEmpty [PrimExpr] -> SqlInsert,
     sqlExpr        :: PrimExpr -> SqlExpr,
     sqlLiteral     :: Literal -> String,
     -- | Turn a string into a quoted string. Quote characters
     -- and any escaping are handled by this function.
     sqlQuote       :: String -> String
    }
