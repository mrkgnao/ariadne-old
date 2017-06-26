{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Edible.Internal.RunQuery where

import           Control.Applicative                  (Applicative, liftA2,
                                                       pure, (*>), (<*>))

import           Database.PostgreSQL.Simple.FromField (FieldParser, FromField,
                                                       fromField,
                                                       pgArrayFieldParser)
import           Database.PostgreSQL.Simple.FromRow   (fieldWith, fromRow)
import           Database.PostgreSQL.Simple.Internal  (RowParser)
import           Database.PostgreSQL.Simple.Types     (Only (..), fromPGArray)

import           Edible.Column                        (Column)
import qualified Edible.Column                        as C
import           Edible.Internal.Column               (Nullable)
import qualified Edible.Internal.PackMap              as PackMap
import qualified Edible.Internal.PGTypes              as IPT (strictDecodeUtf8)
import qualified Edible.Internal.Unpackspec           as U
import qualified Edible.PGTypes                       as T

import           Data.Profunctor                      (dimap)
import qualified Data.Profunctor                      as P
import           Data.Profunctor.Product              (empty, (***!))
import qualified Data.Profunctor.Product              as PP
import qualified Data.Profunctor.Product.Default      as D

import qualified Data.Aeson                           as Ae
import qualified Data.ByteString                      as SBS
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.CaseInsensitive                 as CI
import qualified Data.String                          as String
import qualified Data.Text                            as ST
import qualified Data.Text.Lazy                       as LT
import qualified Data.Time                            as Time
import           Data.UUID                            (UUID)
import           GHC.Int                              (Int32, Int64)

-- { Only needed for annoying postgresql-simple patch below

import           Control.Applicative                  ((<$>))
import           Data.Fixed                           (Fixed (..))
import qualified Data.Fixed                           as Fixed
import           Data.Scientific                      (Scientific)
import qualified Data.Scientific                      as Scientific
import           Data.Typeable                        (Typeable)
import           Database.PostgreSQL.Simple.FromField (ResultError (Incompatible, UnexpectedNull),
                                                       returnError, typeInfo)
import qualified Database.PostgreSQL.Simple.Range     as PGSR
import qualified Database.PostgreSQL.Simple.TypeInfo  as TI

import qualified Database.PostgreSQL.Simple.FromField as Pg
import           GHC.TypeLits
import qualified GHC.TypeLits                         as GHC

-- }

-- | A 'QueryRunnerColumn' @pgType@ @hsType@ encodes how to turn
-- a value of Postgres type @pgType@ into a value of Haskell type
-- @hsType@.  For example a value of type 'QueryRunnerColumn'
-- 'T.PGText' 'String' encodes how to turn a 'T.PGText' result from the
-- database into a Haskell 'String'.

-- This is *not* a Product Profunctor because it is the only way I
-- know of to get the instance generation to work for non-Nullable and
-- Nullable types at once.

-- I can no longer remember what the above comment means, but it might
-- be that we can't add nullability to a RowParser, only to a
-- FieldParser, so we have to have some type that we know contains
-- just a FieldParser.
data QueryRunnerColumn pgType hsType =
  QueryRunnerColumn (U.Unpackspec (Column pgType) ()) (FieldParser hsType)

instance Functor (QueryRunnerColumn u) where
  fmap f ~(QueryRunnerColumn u fp) = QueryRunnerColumn u ((fmap . fmap . fmap) f fp)

-- | A 'QueryRunner' specifies how to convert Postgres values (@columns@)
--   into Haskell values (@haskells@).  Most likely you will never need
--   to create on of these or handle one directly.  It will be provided
--   for you by the 'D.Default' 'QueryRunner' instance.
data QueryRunner columns haskells =
  QueryRunner (U.Unpackspec columns ())
              (columns -> RowParser haskells)
              -- We never actually look at the columns except to see
              -- its "type" in the case of a sum profunctor
              (columns -> Bool)
              -- ^ Have we actually requested any columns?  If we
              -- asked for zero columns then the SQL generator will
              -- have to put a dummy 0 into the SELECT statement,
              -- since we can't select zero columns.  In that case we
              -- have to make sure we read a single Int.
              --
              -- NB this does have to be a function of 'columns'
              -- because we have a `SumProfunctor` instance.  For some
              -- values of 'columns' there may be zero columns and for
              -- other values one or more, for example, 'Maybe (Column
              -- PGInt4)' has no columns when it is Nothing and one
              -- column when it is Just.

fieldQueryRunnerColumn :: FromField haskell => QueryRunnerColumn pgType haskell
fieldQueryRunnerColumn = fieldParserQueryRunnerColumn fromField

fieldParserQueryRunnerColumn :: FieldParser haskell -> QueryRunnerColumn pgType haskell
fieldParserQueryRunnerColumn = QueryRunnerColumn (P.rmap (const ()) U.unpackspecColumn)

queryRunner :: QueryRunnerColumn a b -> QueryRunner (Column a) b
queryRunner qrc = QueryRunner u (const (fieldWith fp)) (const True)
    where QueryRunnerColumn u fp = qrc

queryRunnerColumnNullable :: QueryRunnerColumn a b
                          -> QueryRunnerColumn (Nullable a) (Maybe b)
queryRunnerColumnNullable qr =
  QueryRunnerColumn (P.lmap C.unsafeCoerceColumn u) (fromField' fp)
  where QueryRunnerColumn u fp = qr
        fromField' :: FieldParser a -> FieldParser (Maybe a)
        fromField' _ _ Nothing = pure Nothing
        fromField' fp' f bs    = fmap Just (fp' f bs)

-- { Instances for automatic derivation

instance QueryRunnerColumnDefault a b =>
         QueryRunnerColumnDefault (Nullable a) (Maybe b) where
  queryRunnerColumnDefault = queryRunnerColumnNullable queryRunnerColumnDefault

instance QueryRunnerColumnDefault a b =>
         D.Default QueryRunner (Column a) b where
  def = queryRunner queryRunnerColumnDefault

-- }

-- { Instances that must be provided once for each type.  Instances
--   for Nullable are derived automatically from these.

-- | A 'QueryRunnerColumnDefault' @pgType@ @hsType@ represents
-- the default way to turn a @pgType@ result from the database into a
-- Haskell value of type @hsType@.
--
-- Creating an instance of 'QueryRunnerColumnDefault' for your own types is
-- necessary for retrieving those types from the database.
--
-- You should use one of the three methods below for writing a
-- 'QueryRunnerColumnDefault' instance.
--
-- 1. If you already have a 'FromField' instance for your @hsType@, use
-- 'fieldQueryRunnerColumn'.  (This is how most of the built-in instances are
-- defined.)
--
-- 2. If you don't have a 'FromField' instance, use
-- 'Edible.RunQuery.queryRunnerColumn' if possible.  See the documentation for
-- 'Edible.RunQuery.queryRunnerColumn' for an example.
--
-- 3. If you have a more complicated case, but not a 'FromField' instance,
-- write a 'FieldParser' for your type and use 'fieldParserQueryRunnerColumn'.
-- You can also add a 'FromField' instance using this.
class QueryRunnerColumnDefault pgType hsType where
  queryRunnerColumnDefault :: QueryRunnerColumn pgType hsType

instance QueryRunnerColumnDefault T.PGInt4 Int where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGInt4 Int32 where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGInt8 Int64 where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGText String where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGFloat4 Float where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGFloat8 Double where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGBool Bool where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGUuid UUID where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGBytea SBS.ByteString where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGBytea LBS.ByteString where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGText ST.Text where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGText LT.Text where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGDate Time.Day where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGTimestamptz Time.UTCTime where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGTimestamp Time.LocalTime where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGTime Time.TimeOfDay where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGCitext (CI.CI ST.Text) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGCitext (CI.CI LT.Text) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGJson String where
  queryRunnerColumnDefault = fieldParserQueryRunnerColumn jsonFieldParser

instance QueryRunnerColumnDefault T.PGJson Ae.Value where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault T.PGJsonb String where
  queryRunnerColumnDefault = fieldParserQueryRunnerColumn jsonbFieldParser

instance QueryRunnerColumnDefault T.PGJsonb Ae.Value where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault (T.PGNumeric s) Rational where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
  {-# INLINE queryRunnerColumnDefault #-}

instance QueryRunnerColumnDefault (T.PGNumeric s) Scientific where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
  {-# INLINE queryRunnerColumnDefault #-}

instance QueryRunnerColumnDefault (T.PGNumeric 0) Integer where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
  {-# INLINE queryRunnerColumnDefault #-}

newtype WrapFixed e = WrapFixed { unWrapFixed :: Fixed e }

instance Fixed.HasResolution e => Pg.FromField (WrapFixed e) where
  fromField = fmap (fmap (fmap (WrapFixed . fromRational))) Pg.fromField
  {-# INLINE fromField #-}

instance
  ( Fixed.HasResolution e, GHC.CmpNat s (T.PGNumericScale e + 1) ~ 'LT
  ) => QueryRunnerColumnDefault (T.PGNumeric s) (Fixed e) where
    queryRunnerColumnDefault = fmap unWrapFixed fieldQueryRunnerColumn
    {-# INLINE queryRunnerColumnDefault #-}


-- | Conversions to 'Int' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Int are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Int is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one Int8, Int16, Int32, Int64 from Data.Int.")
  ) => QueryRunnerColumnDefault a Int
  where queryRunnerColumnDefault = undefined

-- | Conversions to 'Word' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Word are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Word is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one of Word8, Word16, Word32 from Data.Word.")
  ) => QueryRunnerColumnDefault a Word
  where queryRunnerColumnDefault = undefined


-- No CI String instance since postgresql-simple doesn't define FromField (CI String)

arrayColumn :: Column (T.PGArray a) -> Column a
arrayColumn = C.unsafeCoerceColumn

instance (Typeable b, QueryRunnerColumnDefault a b) =>
         QueryRunnerColumnDefault (T.PGArray a) [b] where
  queryRunnerColumnDefault = QueryRunnerColumn (P.lmap arrayColumn c) ((fmap . fmap . fmap) fromPGArray (pgArrayFieldParser f))
    where QueryRunnerColumn c f = queryRunnerColumnDefault

-- }

instance (Typeable b, FromField b, QueryRunnerColumnDefault a b) =>
         QueryRunnerColumnDefault (T.PGRange a) (PGSR.PGRange b) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- Boilerplate instances

instance Functor (QueryRunner c) where
  fmap f (QueryRunner u r b) = QueryRunner u ((fmap . fmap) f r) b

-- TODO: Seems like this one should be simpler!
instance Applicative (QueryRunner c) where
  pure = flip (QueryRunner (P.lmap (const ()) PP.empty)) (const False)
         . pure
         . pure
  QueryRunner uf rf bf <*> QueryRunner ux rx bx =
    QueryRunner (P.dimap (\x -> (x,x)) (const ()) (uf PP.***! ux)) ((<*>) <$> rf <*> rx) (liftA2 (||) bf bx)

instance P.Profunctor QueryRunner where
  dimap f g (QueryRunner u r b) =
    QueryRunner (P.lmap f u) (P.dimap f (fmap g) r) (P.lmap f b)

instance PP.ProductProfunctor QueryRunner where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor QueryRunner where
  f +++! g = QueryRunner (P.rmap (const ()) (fu PP.+++! gu))
                         (PackMap.eitherFunction fr gr)
                         (either fb gb)
    where QueryRunner fu fr fb = f
          QueryRunner gu gr gb = g

-- }

-- { Allow @postgresql-simple@ conversions from JSON types to 'String'

jsonFieldParser, jsonbFieldParser :: FieldParser String
jsonFieldParser  = jsonFieldTypeParser (String.fromString "json")
jsonbFieldParser = jsonFieldTypeParser (String.fromString "jsonb")

-- typenames, not type Oids are used in order to avoid creating
-- a dependency on 'Database.PostgreSQL.LibPQ'
jsonFieldTypeParser :: SBS.ByteString -> FieldParser String
jsonFieldTypeParser jsonTypeName field mData = do
    ti <- typeInfo field
    if TI.typname ti == jsonTypeName
       then convert
       else returnError Incompatible field "types incompatible"
  where
    convert = case mData of
        Just bs -> pure $ IPT.strictDecodeUtf8 bs
        _       -> returnError UnexpectedNull field ""

-- }

prepareRowParser :: QueryRunner columns haskells -> columns -> RowParser haskells
prepareRowParser (QueryRunner _ rowParser nonZeroColumns) cols =
  if nonZeroColumns cols
  then rowParser cols
  else (fromRow :: RowParser (Only Int)) *> rowParser cols
     -- If we are selecting zero columns then the SQL
     -- generator will have to put a dummy 0 into the
     -- SELECT statement, since we can't select zero
     -- columns.  In that case we have to make sure we
     -- read a single Int.
