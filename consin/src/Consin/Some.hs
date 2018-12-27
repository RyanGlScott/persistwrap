{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Consin.Some where

import Data.Singletons (Sing, SingI, SingInstance(SingInstance), sing, singInstance)

import Consin.Class (AlwaysS)

data Some f = forall x. SingI x => Some (f x)

some :: Sing x -> f x -> Some f
some (singInstance -> SingInstance) = Some

data GetSome f = forall x. GetSome (Sing x) (f x)

getSome :: Some f -> GetSome f
getSome (Some x) = GetSome sing x

deriving instance AlwaysS Show f => Show (Some f)
