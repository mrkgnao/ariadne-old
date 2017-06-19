{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Lib.Prelude
  ( module Exports
  , (|$)
  , (!$)
  , (|>)
  , (<|)
  , tshow
  , sshow
  , String.fromString
  , (<||)
  , (||>)
  ) where

import           Control.Arrow    as Exports hiding
    ( first
    , second
    , (<<<)
    , (>>>)
    )
import           Control.Category as Exports

import           Control.Lens     as Exports hiding
    ( Strict
    , cons
    , snoc
    , uncons
    , unsnoc
    , (<.>)
    , (<|)
    , (|>)
    , (.=)
    )
import           Protolude        as Exports hiding (from, show, to, (&), (.), Selector)

import qualified Control.Arrow    as Arrow
import qualified Control.Lens     as Lens
import qualified Protolude

import qualified Data.String      as String

infixl 1 |$

-- | Diagrammatic application.
(|$) :: a -> (a -> b) -> b
x |$ f = f x

infixl 1 !$

-- | Strict diagrammatic application.
(!$) :: a -> (a -> b) -> b
x !$ f = f $! x

-- | 'Protolude.show' as any 'IsString' type.
sshow :: (Show a, IsString s) => a -> s
sshow = Protolude.show |> String.fromString

-- | A restricted version for 'Text'.
tshow :: Show a => a -> Text
tshow = sshow

infixr 2 |>

-- | Diagrammatic composition of morphisms.
(|>)
  :: forall cat a b c.
     Category cat
  => cat a b -> cat b c -> cat a c
(|>) = (Arrow.>>>)

infixr 9 <|

-- | Conventional composition of morphisms.
(<|)
  :: forall cat a b c.
     Category cat
  => cat b c -> cat a b -> cat a c
(<|) = (Arrow.<<<)

infixl 5 ||>

-- | Infix alias for 'Lens.|>' to prevent clash with '<|'.
(||>) :: Snoc s s a a => s -> a -> s
(||>) = (Lens.|>)

infixr 5 <||

(<||) :: Cons s s a a => a -> s -> s
(<||) = (Lens.<|)
