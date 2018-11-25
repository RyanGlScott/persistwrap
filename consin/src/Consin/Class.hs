{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Consin.Class where

import Data.Functor.Const (Const)
import Data.Proxy (Proxy)
import Data.Singletons (SingI)
import Generics.Deriving.Eq (GEq, geq)
import Generics.Deriving.Show (GShow, gshowsPrec)

class AlwaysS c f where
  withAlwaysS :: forall x y. SingI x => (c (f x) => y) -> y

instance Eq x => AlwaysS Eq (Const x) where withAlwaysS = id
instance Ord x => AlwaysS Ord (Const x) where withAlwaysS = id
instance AlwaysS Eq Proxy where withAlwaysS = id
instance AlwaysS Ord Proxy where withAlwaysS = id

(==*) :: forall f x . (AlwaysS Eq f, SingI x) => f x -> f x -> Bool
(==*) = withAlwaysS @Eq @f @x (==)

compare1 :: forall f x . (AlwaysS Ord f, SingI x) => f x -> f x -> Ordering
compare1 = withAlwaysS @Ord @f @x compare

showsPrec1 :: forall f x . (AlwaysS Show f, SingI x) => Int -> f x -> ShowS
showsPrec1 = withAlwaysS @Show @f @x showsPrec

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
