{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Consin.SymMap
    ( SymMap
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

newtype SymMap (v :: k -> *) = SymMap (Map.Map (SomeSing k) (Some v))

deriving instance (ShowSing k, AlwaysS Show v) => Show (SymMap (v :: k -> *))

empty :: SymMap v
empty = SymMap Map.empty

singleton :: forall x v . Sing x -> v x -> SymMap v
singleton k = SymMap . Map.singleton (SomeSing k) . some k

union :: SOrd k => SymMap (v :: k -> *) -> SymMap v -> SymMap v
union (SymMap x) (SymMap y) = SymMap $ Map.union x y

difference, (\\) :: SOrd k => SymMap (v :: k -> *) -> SymMap v -> SymMap v
difference (SymMap x) (SymMap y) = SymMap $ Map.difference x y
(\\) = difference

insert :: forall k (x :: k) v . SOrd k => Sing x -> v x -> SymMap v -> SymMap v
insert k v (SymMap m) = SymMap $ Map.insert (SomeSing k) (some k v) m

delete :: SOrd k => Sing (x :: k) -> SymMap (v :: k -> *) -> SymMap v
delete sx (SymMap m) = SymMap $ Map.delete (SomeSing sx) m

lookup :: forall k (x :: k) v . (SDecide k, SOrd k) => Sing x -> SymMap v -> Maybe (v x)
lookup k (SymMap m) =
  (\(Some (v :: v x')) -> case sing @_ @x' %~ k of
      Disproved{} -> error "Bad value in Map"
      Proved Refl -> v
    )
    <$> Map.lookup (SomeSing k) m

(!?) :: forall k (x :: k) v . (SDecide k, SOrd k) => SymMap v -> Sing x -> Maybe (v x)
(!?) = flip lookup

fromList :: forall k (v :: k -> *) . SOrd k => [Some v] -> SymMap v
fromList = SymMap . Map.fromList . map (\(Some (v :: v x)) -> (SomeSing $ sing @_ @x, Some v))

toList :: forall v . SymMap v -> [Some v]
toList (SymMap m) = map snd $ Map.toList m
