{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Miscellaneous compatibility stuff.
module Tisch.Internal.Compat
  ( AnyColumn(..)
  , unsafeFunExpr
  , unsafeUnNullableColumn
  , pgFixed
  ) where

import qualified Control.Exception                    as Ex
import           Data.Fixed                           (Fixed (..))
import qualified Data.Fixed                           as Fixed
import           Data.Int
import           Data.Maybe
import           Data.Proxy
import           Data.Scientific                      (Scientific,
                                                       formatScientific)
import qualified Data.Scientific                      as Scientific
import qualified Database.PostgreSQL.Simple.FromField as Pg
import           GHC.Float                            (float2Double)
import           GHC.Real                             (infinity, notANumber)
import           GHC.TypeLits                         (type (+), KnownNat, Nat)
import qualified GHC.TypeLits                         as GHC

import qualified Edible.Column                        as O
import qualified Edible.Order                         as O
import qualified Edible.PGTypes                       as O
import qualified Edible.RunQuery                      as O

import qualified Edible.Internal.Column               as OI
import qualified Edible.Internal.HaskellDB.PrimQuery  as HDB
import qualified Edible.Internal.PGTypes              as OI
import qualified Edible.Internal.RunQuery             as OI

--------------------------------------------------------------------------------

data AnyColumn = forall a. AnyColumn (O.Column a)

-- | 'unsafeFunExpr "f" xs' calls a function called @"f"@ with arguments @xs@.
-- The return type must correctly be set by the caller.
unsafeFunExpr :: HDB.Name -> [AnyColumn] -> O.Column b
unsafeFunExpr fname =
  OI.Column . HDB.FunExpr fname . map (\(AnyColumn (OI.Column x)) -> x)

unsafeUnNullableColumn :: O.Column (O.Nullable a) -> O.Column a
unsafeUnNullableColumn = O.unsafeCoerceColumn

-- | Orphan. "Tisch.Internal".
instance OI.QueryRunnerColumnDefault O.PGFloat4 Float where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

-- | Conversions to 'Int' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Int are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Int is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one Int8, Int16, Int32, Int64 from Data.Int.")
  ) => OI.QueryRunnerColumnDefault a Int
  where queryRunnerColumnDefault = undefined

-- | Conversions to 'Word' are explicitely disabled.
instance {-# OVERLAPPING #-}
  ( GHC.TypeError
      ('GHC.Text "QueryRunnerColumnDefault conversions to Word are disabled because the size"
       'GHC.:$$: 'GHC.Text "of Word is machine-dependent, which is likely to cause you maintenance"
       'GHC.:$$: 'GHC.Text "problems in the future. Be explicit about the size of your integer,"
       'GHC.:$$: 'GHC.Text "use one of Word8, Word16, Word32 from Data.Word.")
  ) => OI.QueryRunnerColumnDefault a Word
  where queryRunnerColumnDefault = undefined

--------------------------------------------------------------------------------

-- | PostgreSQL @numeric@ type, with @scale@ indicating how many decimal digits
-- does this @numeric@ value support.
--
-- Note that @scale@ is a phantom types are only ever used in the Haskell side,
-- and never on the PostgreSQL side. That is, a @'PGRawNumeric' s@ type in
-- Haskell maps to a @numeric@ type without a scale specified.
--
-- 'PGRawNumeric' doesn't support specifying the “precision” of the PostgreSQL
-- @numeric@ type, as there's no use for that precision on the Haskell side and
-- we always support the full precision.

pgFixed
  :: forall e s
  .  ( KnownNat s
     , Fixed.HasResolution e, GHC.CmpNat s (O.PGNumericScale e + 1) ~ 'LT)
  => Fixed e -> O.Column (O.PGNumeric s)
pgFixed = case GHC.natVal (Proxy :: Proxy s) of
  0 -> \(MkFixed x) -> OI.literalColumn (HDB.IntegerLit x)
  _ -> OI.literalColumn . HDB.OtherLit . Fixed.showFixed False
{-# INLINE pgFixed #-}

newtype WrapFixed e = WrapFixed { unWrapFixed :: Fixed e }

instance Fixed.HasResolution e => Pg.FromField (WrapFixed e) where
  fromField = fmap (fmap (fmap (WrapFixed . fromRational))) Pg.fromField
  {-# INLINE fromField #-}

instance
  ( Fixed.HasResolution e, GHC.CmpNat s (O.PGNumericScale e + 1) ~ 'LT
  ) => OI.QueryRunnerColumnDefault (O.PGNumeric s) (Fixed e) where
    queryRunnerColumnDefault = fmap unWrapFixed O.fieldQueryRunnerColumn
    {-# INLINE queryRunnerColumnDefault #-}

