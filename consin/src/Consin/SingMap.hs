{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Consin.SingMap
    ( SingMap
    , (\\)
    , (!?)
    , delete
    , difference
    , empty
    , fromList
    , insert
    , lookup
    , singleton
    , toList
    , union
    ) where

import Prelude hiding (lookup)

import Data.Kind (type (*))
import qualified Data.Map as Map
import Data.Singletons (Sing, SomeSing(SomeSing), sing)
import Data.Singletons.Decide ((:~:)(Refl), Decision(..), SDecide, (%~))
import Data.Singletons.ShowSing (ShowSing)
import Data.Singletons.Prelude (SOrd)

import Consin.Class (AlwaysS)
import Consin.Some

newtype SingMap (v :: k -> *) = SingMap (Map.Map (SomeSing k) (Some v))

deriving instance (ShowSing k, AlwaysS Show v) => Show (SingMap (v :: k -> *))

empty :: SingMap v
empty = SingMap Map.empty

singleton :: forall x v . Sing x -> v x -> SingMap v
singleton k = SingMap . Map.singleton (SomeSing k) . some k

union :: SOrd k => SingMap (v :: k -> *) -> SingMap v -> SingMap v
union (SingMap x) (SingMap y) = SingMap $ Map.union x y

difference, (\\) :: SOrd k => SingMap (v :: k -> *) -> SingMap v -> SingMap v
difference (SingMap x) (SingMap y) = SingMap $ Map.difference x y
(\\) = difference

insert :: forall k (x :: k) v . SOrd k => Sing x -> v x -> SingMap v -> SingMap v
insert k v (SingMap m) = SingMap $ Map.insert (SomeSing k) (some k v) m

delete :: SOrd k => Sing (x :: k) -> SingMap (v :: k -> *) -> SingMap v
delete sx (SingMap m) = SingMap $ Map.delete (SomeSing sx) m

lookup :: forall k (x :: k) v . (SDecide k, SOrd k) => Sing x -> SingMap v -> Maybe (v x)
lookup k (SingMap m) =
  (\(Some (v :: v x')) -> case sing @_ @x' %~ k of
      Disproved{} -> error "Bad value in Map"
      Proved Refl -> v
    )
    <$> Map.lookup (SomeSing k) m

(!?) :: forall k (x :: k) v . (SDecide k, SOrd k) => SingMap v -> Sing x -> Maybe (v x)
(!?) = flip lookup

fromList :: forall k (v :: k -> *) . SOrd k => [Some v] -> SingMap v
fromList = SingMap . Map.fromList . map (\(Some (v :: v x)) -> (SomeSing $ sing @_ @x, Some v))

toList :: forall v . SingMap v -> [Some v]
toList (SingMap m) = map snd $ Map.toList m
