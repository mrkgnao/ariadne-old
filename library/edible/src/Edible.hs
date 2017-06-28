-- This is the entry point for the @tisch@ library.
--
-- This module re-exports much, but not all of the @Edible.Run@ and
-- @Edible.Internal.*@ modules. If you are looking for some tool that you
-- can't find here, please refer to those modules instead.
--
-- This module doesn't export any infix operator, and likely never will.
module Edible
 ( -- * Running queries
   --
   -- $runningQueries
   runQuery
 , runQuery1
 , runInsert
 , runInsert1
 , runUpdate
 , runDelete
 , runInsertReturning
 , runInsertReturning1
   -- * Connection management
 , Perm(..)
 , Allow
 , Forbid
 , Conn
 , Conn'
 , connect
 , connect'
 , close
 , withReadOnlyTransaction
 , withReadWriteTransaction
 , withSavepoint
   -- * Defining a 'Table'
 , Table
 , TableR
 , TableRW
 , Database
 , SchemaName
 , TableName
 , Columns
 , Column(..)
 , RCap(..)
 , WCap(..)
   -- * Working with 'Table'
 , HsR
 , HsI
 , mkHsI
 , hsi
 , WDef(..)
 , wdef
 , PgR
 , PgRN
 , PgW
   -- * Kol
 , Kol
 , ToKol(..)
 , liftKol1
 , liftKol2
   -- * Koln
 , Koln
 , koln
 , nul
 , fromKol
 , fromKoln
 , matchKoln
 , isNull
 , mapKoln
 , forKoln
 , bindKoln
 , altKoln
   -- * Querying
 , Query.Query
 , Query.query
 , Query.innerJoin
 , Query.leftJoin
 , restrict
 , Query.restrictf
 , distinct
 , Query.limit
 , Query.offset
   -- * Selecting
 , col
   -- * Ordering
 , E.Order
 , Query.orderBy
 , Query.asc
 , Query.ascNullsFirst
 , Query.ascNullsLast
 , Query.desc
 , Query.descNullsFirst
 , Query.descNullsLast
   -- * Operators
 , lnot
 , lor
 , land
 , matchBool
   -- ** Equality
 , PgEq
 , eq
 , member
 , Query.memberq
   -- ** Comparisons
 , PgOrd
 , lt
 , lte
 , gt
 , gte
   -- ** Various numeric
 , PgNum
 , PgIntegral
 , PgFractional
 , PgFloating
 , modulo
 , euler's
 , itruncate
 , iround
 , iceil
 , ifloor
   -- ** Bit-wise
 , PgBitwise
 , bwand
 , bwor
 , bwxor
 , bwnot
 , bwsl
 , bwsr
   -- ** Time
 , nowTransaction
 , nowStatement
 , nowClock
 , toTimestamptz
 , toTimestamp
 , tstzEpoch
 , tsCentury
 , tsDay
 , tsDayOfTheWeek
 , tsDayOfTheWeekISO8601
 , tsDayOfTheYear
 , tsDecade
 , tsHour
 , tsMicroseconds
 , tsMillenium
 , tsMilliseconds
 , tsMinute
 , tsMonth
 , tsQuarter
 , tsSecond
 , tsWeekISO8601
 , tsYear
 , tsYearISO8601
   -- ** Regular expressions
 , reSub
 , reReplace
 , reReplaceg
 , reMatch
 , Query.reMatches
 , reSplit
   -- * Aggregation
   --
   -- $aggregation
 , Aggregator
 , orderAggregator
 , aggregate
 , groupBy
 , AggSum
 , sumgg
 , countgg
 , countngg
 , countRows
 , AggAvg
 , avggg
 , bwandgg
 , bworgg
 , landgg
 , lorgg
 , maxgg
 , mingg
 , arraygg
 , arrayngg
 , jsonarraygg
 , jsonbarraygg
 , textgg
 , byteagg
 , AggStdDev
 , stddevgg
 , stddevpopgg
 , variancegg
 , variancepopgg
   -- * Column types
 , PgTyped(..)
 , E.PGArray
 , PGArrayn
 , E.PGBool
 , E.PGBytea
 , E.PGCitext
 , E.PGDate
 , E.PGFloat4
 , E.PGFloat8
 , E.PGInt2
 , E.PGInt4
 , E.PGInt8
 , E.PGJsonb
 , E.PGJson
 , E.PGNumeric
 , E.PGText
 , E.PGTimestamptz
 , E.PGTimestamp
 , E.PGTime
 , E.PGUuid
   -- ** Parsing
 , QueryRunnerColumnDefault(..)
 , qrcFromField
 , qrcFieldParser
 , qrcFieldParserMap
 , qrcMap
 , qrcMapMay
 , qrcPrism
 , qrcWrapped
   -- ** Coercing / type casting
 , CastKol
 , castKol
 , upcastKol
 , unsafeDowncastKol
 , unsafeCoerceKol
 , unsaferCoerceKol
 , unsaferCastKol
 ) where

import           Edible.Aggregate
import           Edible.Internal.Kol
import           Edible.Internal.Koln
import qualified Edible.Internal.Query as Query
import           Edible.Internal.Table
import           Edible.Operators

import qualified Edible.Order          as E
import qualified Edible.PGTypes        as E
import           Edible.Run

-- import qualified Edible.Aggregate           as E
-- import qualified Edible.Binary              as E
-- import qualified Edible.Column              as E
-- import qualified Edible.Constant            as E
-- import qualified Edible.Distinct            as E
-- import qualified Edible.FunctionalJoin      as E
-- import qualified Edible.Join                as E
-- import qualified Edible.Label               as E
-- import qualified Edible.Manipulation        as E
-- import qualified Edible.Operators           as E
-- import qualified Edible.QueryArr            as E
-- import qualified Edible.RunQuery            as E
-- import qualified Edible.Sql                 as E
-- import qualified Edible.Table               as E
-- import qualified Edible.Values              as E

-- $runningQueries
--
-- The "Edible.Run" module exports lower-level variants of these @runXxx@
-- functions, in case you need those.


-- $aggregation
--
-- Except for 'groupBy', all aggregation functions working on 'Kol's are
-- suffixed with @gg@ (for a/gg/regate). Aggregation functions working on
-- 'Koln's are suffixed with @ngg@.
