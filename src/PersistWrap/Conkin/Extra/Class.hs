{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra.Class where

import Data.Constraint (Dict (Dict))
import Data.Functor.Const (Const)
import Data.Proxy (Proxy)
import Data.Singletons (SingI)

class Always c f where
  dict :: SingI x => Dict (c (f x))

instance Eq x => Always Eq (Const x) where dict = Dict
instance Ord x => Always Ord (Const x) where dict = Dict
instance Always Eq Proxy where dict = Dict
instance Always Ord Proxy where dict = Dict

(==*) :: forall f x. (Always Eq f, SingI x) => f x -> f x -> Bool
(==*) = case dict :: Dict (Eq (f x)) of Dict -> (==)

compare1 :: forall f x. (Always Ord f, SingI x) => f x -> f x -> Ordering
compare1 = case (dict :: Dict (Ord (f x))) of Dict -> compare

showsPrec1 :: forall f x. (Always Show f, SingI x) => Int -> f x -> ShowS
showsPrec1 = case (dict :: Dict (Show (f x))) of Dict -> showsPrec
