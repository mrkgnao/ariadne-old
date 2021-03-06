{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Postgres types and functions to create 'Column's of those types.
-- You may find it more convenient to use "Edible.Constant" instead.

module Edible.PGTypes (module Edible.PGTypes) where

import qualified Control.Exception                     as Ex

import           Edible.Internal.Column                (Column)
import qualified Edible.Internal.Column                as C
import qualified Edible.Internal.PGTypes               as IPT

import qualified Edible.Internal.HaskellDB.PrimQuery   as HPQ
import qualified Edible.Internal.HaskellDB.Sql.Default as HSD

import qualified Data.Aeson                            as Ae
import qualified Data.ByteString                       as SByteString
import qualified Data.ByteString.Lazy                  as LByteString
import qualified Data.CaseInsensitive                  as CI
import qualified Data.Text                             as SText
import qualified Data.Text.Lazy                        as LText
import qualified Data.Time                             as Time
import qualified Data.UUID                             as UUID

import           Data.Fixed                            (Fixed (..))
import qualified Data.Fixed                            as Fixed
import           Data.Int
import           GHC.Float                             (float2Double)
import           GHC.Real                              (infinity, notANumber)

import qualified Database.PostgreSQL.Simple.Range      as R

import           Data.Scientific                       (Scientific,
                                                        formatScientific)
import           Data.Scientific                       as Scientific

import           Data.Maybe                            (fromMaybe)
import           Data.Proxy
import           GHC.TypeLits

instance C.PGNum PGFloat8 where
  pgFromInteger = pgDouble . fromInteger

instance C.PGNum PGInt4 where
  pgFromInteger = pgInt4 . fromInteger

instance C.PGNum PGInt8 where
  pgFromInteger = pgInt8 . fromInteger

instance C.PGFractional PGFloat8 where
  pgFromRational = pgDouble . fromRational

-- | Orphan. "Tisch.Internal".
instance C.PGFractional PGFloat4 where
  pgFromRational = pgFloat4 . fromRational

pgFloat4 :: Float -> Column PGFloat4
pgFloat4 = literalColumn . HPQ.DoubleLit . float2Double

pgFloat8 :: Float -> Column PGFloat8
pgFloat8 = literalColumn . HPQ.DoubleLit . float2Double

pgInt2 :: Int16 -> Column PGInt2
pgInt2 = literalColumn . HPQ.IntegerLit . fromIntegral


-- | Orphan. "Tisch.Internal".
instance C.PGNum PGFloat4 where
  pgFromInteger = pgFloat4 . fromInteger

-- | Orphan. "Tisch.Internal".
instance C.PGNum PGInt2 where
  pgFromInteger = pgInt2 . fromInteger

instance C.PGIntegral PGInt2
instance C.PGIntegral PGInt4
instance C.PGIntegral PGInt8

instance C.PGString PGText where
  pgFromString = pgString

instance C.PGString PGCitext where
  pgFromString = pgCiLazyText . CI.mk . LText.pack

-- * Creating SQL values

pgString :: String -> Column PGText
pgString = IPT.literalColumn . HPQ.StringLit

pgLazyByteString :: LByteString.ByteString -> Column PGBytea
pgLazyByteString = IPT.literalColumn . HPQ.ByteStringLit . LByteString.toStrict

pgStrictByteString :: SByteString.ByteString -> Column PGBytea
pgStrictByteString = IPT.literalColumn . HPQ.ByteStringLit

pgStrictText :: SText.Text -> Column PGText
pgStrictText = IPT.literalColumn . HPQ.StringLit . SText.unpack

pgLazyText :: LText.Text -> Column PGText
pgLazyText = IPT.literalColumn . HPQ.StringLit . LText.unpack

pgInt4 :: Int -> Column PGInt4
pgInt4 = IPT.literalColumn . HPQ.IntegerLit . fromIntegral

pgInt8 :: Int64 -> Column PGInt8
pgInt8 = IPT.literalColumn . HPQ.IntegerLit . fromIntegral

pgDouble :: Double -> Column PGFloat8
pgDouble = IPT.literalColumn . HPQ.DoubleLit

pgBool :: Bool -> Column PGBool
pgBool = IPT.literalColumn . HPQ.BoolLit

pgUUID :: UUID.UUID -> Column PGUuid
pgUUID = C.unsafeCoerceColumn . pgString . UUID.toString

pgDay :: Time.Day -> Column PGDate
pgDay = IPT.unsafePgFormatTime "date" "'%F'"

pgUTCTime :: Time.UTCTime -> Column PGTimestamptz
pgUTCTime = IPT.unsafePgFormatTime "timestamptz" "'%FT%T%QZ'"

pgLocalTime :: Time.LocalTime -> Column PGTimestamp
pgLocalTime = IPT.unsafePgFormatTime "timestamp" "'%FT%T%Q'"

pgTimeOfDay :: Time.TimeOfDay -> Column PGTime
pgTimeOfDay = IPT.unsafePgFormatTime "time" "'%T%Q'"

-- "We recommend not using the type time with time zone"
-- http://www.postgresql.org/docs/8.3/static/datatype-datetime.html


pgCiStrictText :: CI.CI SText.Text -> Column PGCitext
pgCiStrictText = IPT.literalColumn . HPQ.StringLit . SText.unpack . CI.original

pgCiLazyText :: CI.CI LText.Text -> Column PGCitext
pgCiLazyText = IPT.literalColumn . HPQ.StringLit . LText.unpack . CI.original

-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

-- The json data type was introduced in PostgreSQL version 9.2
-- JSON values must be SQL string quoted
pgJSON :: String -> Column PGJson
pgJSON = IPT.castToType "json" . HSD.quote

pgStrictJSON :: SByteString.ByteString -> Column PGJson
pgStrictJSON = pgJSON . IPT.strictDecodeUtf8

pgLazyJSON :: LByteString.ByteString -> Column PGJson
pgLazyJSON = pgJSON . IPT.lazyDecodeUtf8

pgValueJSON :: Ae.ToJSON a => a -> Column PGJson
pgValueJSON = pgLazyJSON . Ae.encode

-- The jsonb data type was introduced in PostgreSQL version 9.4
-- JSONB values must be SQL string quoted
--
-- TODO: We need to add literal JSON and JSONB types so we can say
-- `castToTypeTyped JSONB` rather than `castToType "jsonb"`.
pgJSONB :: String -> Column PGJsonb
pgJSONB = IPT.castToType "jsonb" . HSD.quote

pgStrictJSONB :: SByteString.ByteString -> Column PGJsonb
pgStrictJSONB = pgJSONB . IPT.strictDecodeUtf8

pgLazyJSONB :: LByteString.ByteString -> Column PGJsonb
pgLazyJSONB = pgJSONB . IPT.lazyDecodeUtf8

pgValueJSONB :: Ae.ToJSON a => a -> Column PGJsonb
pgValueJSONB = pgLazyJSONB . Ae.encode

pgArray :: forall a b. IsSqlType b => (a -> C.Column b) -> [a] -> C.Column (PGArray b)
pgArray pgEl xs = C.unsafeCast arrayTy $
  C.Column (HPQ.ArrayExpr (map oneEl xs))
  where
    oneEl = C.unColumn . pgEl
    arrayTy = showSqlType ([] :: [PGArray b])

pgRange :: forall a b. IsRangeType b => (a -> C.Column b) -> R.RangeBound a -> R.RangeBound a -> C.Column (PGRange b)
pgRange pgEl start end = C.Column (HPQ.CastExpr (showRangeType ([] :: [b])) $ HPQ.RangeExpr (oneEl start) (oneEl end))
  where oneEl (R.Inclusive a) = HPQ.Inclusive . C.unColumn $ pgEl a
        oneEl (R.Exclusive a) = HPQ.Exclusive . C.unColumn $ pgEl a
        oneEl (R.NegInfinity) = HPQ.NegInfinity
        oneEl (R.PosInfinity) = HPQ.PosInfinity

class IsSqlType pgType where
  -- | 'showSqlType' will be deprecated in version 0.6.  Use
  -- 'showSqlType' instead.
  showPGType :: proxy pgType -> String
  showPGType  = showSqlType

  showSqlType :: proxy pgType -> String
  showSqlType = showPGType

instance IsSqlType PGBool where
  showSqlType _ = "boolean"
instance IsSqlType PGDate where
  showSqlType _ = "date"
instance IsSqlType PGFloat4 where
  showSqlType _ = "real"
instance IsSqlType PGFloat8 where
  showSqlType _ = "double precision"
instance IsSqlType PGInt8 where
  showSqlType _ = "bigint"
instance IsSqlType PGInt4 where
  showSqlType _ = "integer"
instance IsSqlType PGInt2 where
  showSqlType _ = "smallint"
instance IsSqlType PGRawNumeric where
  showSqlType _ = "numeric"
instance IsSqlType PGText where
  showSqlType _ = "text"
instance IsSqlType PGTime where
  showSqlType _ = "time"
instance IsSqlType PGTimestamp where
  showSqlType _ = "timestamp"
instance IsSqlType PGTimestamptz where
  showSqlType _ = "timestamp with time zone"
instance IsSqlType PGUuid where
  showSqlType _ = "uuid"
instance IsSqlType PGCitext where
  showSqlType _ =  "citext"
instance IsSqlType PGBytea where
  showSqlType _ = "bytea"
instance (IsSqlType (a :: *)) => IsSqlType (PGArray a) where
  showSqlType _ = showSqlType ([] :: [a]) ++ "[]"
instance IsSqlType a => IsSqlType (C.Nullable a) where
  showSqlType _ = showSqlType ([] :: [a])
instance IsSqlType PGJson where
  showSqlType _ = "json"
instance IsSqlType PGJsonb where
  showSqlType _ = "jsonb"
instance (IsRangeType (a :: *)) => IsSqlType (PGRange a) where
  showSqlType _ = showRangeType ([] :: [a])

class IsSqlType pgType => IsRangeType pgType where
  showRangeType :: proxy pgType -> String

instance IsRangeType PGInt4 where
  showRangeType _ = "int4range"

instance IsRangeType PGInt8 where
  showRangeType _ = "int8range"

instance IsRangeType PGRawNumeric where
  showRangeType _ = "numrange"

instance IsRangeType PGTimestamp where
  showRangeType _ = "tsrange"

instance IsRangeType PGTimestamptz where
  showRangeType _ = "tstzrange"

instance IsRangeType PGDate where
  showRangeType _ = "daterange"

-- * SQL datatypes

data PGBool
data PGDate
data PGFloat4
data PGFloat8
data PGInt8
data PGInt4
data PGInt2
data PGRawNumeric
data PGNumeric (scale :: Nat)
data PGText
data PGTime
data PGTimestamp
data PGTimestamptz
data PGUuid
data PGCitext
data PGArray a
data PGBytea
data PGJson
data PGJsonb
data PGRange a

type family PGNumericScale (t :: k) :: Nat
type instance PGNumericScale Fixed.E0  = 0
type instance PGNumericScale Fixed.E1  = 1
type instance PGNumericScale Fixed.E2  = 2
type instance PGNumericScale Fixed.E3  = 3
type instance PGNumericScale Fixed.E6  = 6
type instance PGNumericScale Fixed.E9  = 9
type instance PGNumericScale Fixed.E12 = 12

instance KnownNat s => C.PGNum (PGNumeric s) where
  pgFromInteger = pgScientific . fromInteger
  {-# INLINE pgFromInteger #-}

-- | WARNING: 'pgFromRational' throws 'Ex.RatioZeroDenominator' if given a
-- positive or negative 'infinity'.
instance KnownNat s => C.PGFractional (PGNumeric s) where
  pgFromRational = fromMaybe (Ex.throw Ex.RatioZeroDenominator) . pgRational
  {-# INLINE pgFromRational #-}

-- | Convert a 'Rational' to a @numeric@ column in PostgreSQL. 'notANumber' is
-- supported.
--
-- Returns 'Nothing' in case of positive or negative 'infinity'.
pgRational :: KnownNat s => Rational -> Maybe (C.Column (PGNumeric s))
pgRational x
  | x == infinity    = Nothing
  | x == (-infinity) = Nothing
  | x == notANumber  = Just (literalColumn (HPQ.StringLit "NaN"))
  | otherwise        = Just (pgScientific (fromRational x))


pgFixed
  :: forall e s
  .  ( KnownNat s
     , Fixed.HasResolution e, CmpNat s (PGNumericScale e + 1) ~ 'LT)
  => Fixed e -> C.Column (PGNumeric s)
pgFixed = case natVal (Proxy :: Proxy s) of
  0 -> \(MkFixed x) -> literalColumn (HPQ.IntegerLit x)
  _ -> literalColumn . HPQ.OtherLit . Fixed.showFixed False
{-# INLINE pgFixed #-}

pgScientific :: forall s. KnownNat s => Scientific -> Column (PGNumeric s)
pgScientific = literalColumn . HPQ.OtherLit . formatScientific
  Scientific.Fixed (Just (fromInteger (natVal (Proxy :: Proxy s))))
{-# INLINE pgScientific #-}

-- * Deprecated functions

literalColumn :: HPQ.Literal -> Column a
literalColumn = IPT.literalColumn
{-# WARNING literalColumn
    "'literalColumn' has been moved to Edible.Internal.PGTypes and will be deprecated in version 0.6"
  #-}

unsafePgFormatTime :: Time.FormatTime t => HPQ.Name -> String -> t -> Column c
unsafePgFormatTime = IPT.unsafePgFormatTime
{-# WARNING unsafePgFormatTime
    "'unsafePgFormatTime' has been moved to Edible.Internal.PGTypes and will be deprecated in version 0.6"
  #-}
