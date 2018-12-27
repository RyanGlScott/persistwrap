{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Consin.Class where

import Prelude hiding (Functor(..))

import Data.Singletons (Sing, SingI, SingInstance(..), sing, singInstance)
import Generics.Deriving.Eq (GEq, geq)
import Generics.Deriving.Show (GShow, gshowsPrec)

class (forall x. SingI x => c (f x)) => AlwaysS c f
instance (forall x. SingI x => c (f x)) => AlwaysS c f

withAlwaysS :: forall c f x y. AlwaysS c f => Sing x -> (c (f x) => y) -> y
withAlwaysS (singInstance -> SingInstance) y = y

(==*) :: forall f x . (AlwaysS Eq f, SingI x) => f x -> f x -> Bool
(==*) = withAlwaysS @Eq @f @x sing (==)

compare1 :: forall f x . (AlwaysS Ord f, SingI x) => f x -> f x -> Ordering
compare1 = withAlwaysS @Ord @f @x sing compare

showsPrec1 :: forall f x . (AlwaysS Show f, SingI x) => Int -> f x -> ShowS
showsPrec1 = withAlwaysS @Show @f @x sing showsPrec

newtype FromAlwaysS f x = FromAlwaysS (f x)
instance (AlwaysS Eq f, SingI x) => Eq (FromAlwaysS f x) where
  (==) (FromAlwaysS l) (FromAlwaysS r) = l ==* r
instance (AlwaysS Eq f, SingI x) => GEq (FromAlwaysS f x) where
  geq = (==)
instance (AlwaysS Eq f, AlwaysS Ord f, SingI x) => Ord (FromAlwaysS f x) where
  compare (FromAlwaysS l) (FromAlwaysS r) = compare1 l r
instance (AlwaysS Show f, SingI x) => Show (FromAlwaysS f x) where
  showsPrec d (FromAlwaysS x) = showsPrec1 d x
instance (AlwaysS Show f, SingI x) => GShow (FromAlwaysS f x) where
  gshowsPrec = showsPrec

class Functor f where
  fmapSing :: forall a b . (forall x . SingI x => a x -> b x) -> f a -> f b

class ConsinShow x where
  showsPrecS :: Int -> x -> ShowS

newtype ConsinC x = ConsinC x
instance ConsinShow x => Show (ConsinC x) where
  showsPrec d (ConsinC x) = showsPrecS d x
