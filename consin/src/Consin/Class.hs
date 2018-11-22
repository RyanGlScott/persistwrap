{-# LANGUAGE PolyKinds #-}

module Consin.Class where

import Data.Constraint (Dict(Dict))
import Data.Functor.Const (Const)
import Data.Proxy (Proxy)
import Data.Singletons (SingI)
import Generics.Deriving.Eq (GEq, geq)
import Generics.Deriving.Show (GShow, gshowsPrec)

class AlwaysS c f where
  dictS :: SingI x => Dict (c (f x))

instance Eq x => AlwaysS Eq (Const x) where dictS = Dict
instance Ord x => AlwaysS Ord (Const x) where dictS = Dict
instance AlwaysS Eq Proxy where dictS = Dict
instance AlwaysS Ord Proxy where dictS = Dict

(==*) :: forall f x . (AlwaysS Eq f, SingI x) => f x -> f x -> Bool
(==*) = case dictS :: Dict (Eq (f x)) of
  Dict -> (==)

compare1 :: forall f x . (AlwaysS Ord f, SingI x) => f x -> f x -> Ordering
compare1 = case (dictS :: Dict (Ord (f x))) of
  Dict -> compare

showsPrec1 :: forall f x . (AlwaysS Show f, SingI x) => Int -> f x -> ShowS
showsPrec1 = case (dictS :: Dict (Show (f x))) of
  Dict -> showsPrec

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
