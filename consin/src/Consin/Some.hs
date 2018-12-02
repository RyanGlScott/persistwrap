{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Consin.Some where

import Data.Constraint (Dict(Dict))
import Data.Function.Pointless ((.:))
import Data.Kind (type (*))
import Data.Maybe (isJust)
import Data.Singletons (Sing, SingI, SingInstance(SingInstance), sing, singInstance)
import Data.Singletons.Decide

import Consin.Class (AlwaysS, (==*), showsPrec1)

data Some f = forall x. SingI x => Some (f x)

some :: Sing x -> f x -> Some f
some (singInstance -> SingInstance) = Some

data GetSome f = forall x. GetSome (Sing x) (f x)

getSome :: Some f -> GetSome f
getSome (Some x) = GetSome sing x

class HEq (f :: k -> *) where
  heq :: forall x y. (SingI x, SingI y) => f x -> f y -> Maybe (Dict (x ~ y))
  (==^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (==^) = isJust .: heq
  (/=^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (/=^) = not .: (==^)

instance (SDecide k, AlwaysS Eq f) => HEq (f :: k -> *) where
  heq :: forall x y. (SingI x, SingI y) => f x -> f y -> Maybe (Dict (x ~ y))
  heq = case sing @_ @x %~ sing @_ @y of
    Proved Refl -> \x y -> if x ==* y then Just Dict else Nothing
    Disproved{} -> \_ _ -> Nothing

instance HEq f => Eq (Some f) where
  (==) (Some x) (Some y) = x ==^ y
  (/=) (Some x) (Some y) = x /=^ y

instance AlwaysS Show f => Show (Some f) where
  showsPrec d (Some x) = showParen (d > 10) $ showString "Some " . showsPrec1 11 x
