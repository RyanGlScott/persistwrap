{-# OPTIONS_GHC -Wno-orphans #-}

module PersistWrap.Structure.Orphans () where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Generics.Deriving.Eq (GEq(..))
import Generics.Deriving.Show (GShow(..))

import PersistWrap.Primitives (PrimType, SPrimName, SingPrim, deriveConstraint)

instance GEq ByteString where
  geq = (==)
instance GEq Text where
  geq = (==)
instance GEq Rational where
  geq = (==)
instance GEq Day where
  geq = (==)
instance GEq SingPrim where
  geq = (==)
instance GEq TimeOfDay where
  geq = (==)
instance GEq UTCTime where
  geq = (==)
-- Ensures we don't miss any
_deriveGEqPrimitive :: SPrimName p -> (GEq (PrimType p) => y) -> y
_deriveGEqPrimitive = deriveConstraint @GEq

instance GShow ByteString where
  gshowsPrec = showsPrec
instance GShow Text where
  gshowsPrec = showsPrec
instance GShow Rational where
  gshowsPrec = showsPrec
instance GShow Day where
  gshowsPrec = showsPrec
instance GShow SingPrim where
  gshowsPrec = showsPrec
instance GShow TimeOfDay where
  gshowsPrec = showsPrec
instance GShow UTCTime where
  gshowsPrec = showsPrec
-- Ensures we don't miss any
_deriveGShowPrimitive :: SPrimName p -> (GShow (PrimType p) => y) -> y
_deriveGShowPrimitive = deriveConstraint @GShow
