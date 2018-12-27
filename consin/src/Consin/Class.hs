{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Consin.Class where

import Prelude hiding (Functor(..))

import Data.Singletons (SingI)

class (forall x. SingI x => c (f x)) => AlwaysS c f
instance (forall x. SingI x => c (f x)) => AlwaysS c f

class Functor f where
  fmapSing :: forall a b . (forall x . SingI x => a x -> b x) -> f a -> f b

class ConsinShow x where
  showsPrecS :: Int -> x -> ShowS

newtype ConsinC x = ConsinC x
instance ConsinShow x => Show (ConsinC x) where
  showsPrec d (ConsinC x) = showsPrecS d x
