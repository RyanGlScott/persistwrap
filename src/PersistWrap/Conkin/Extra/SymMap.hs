module PersistWrap.Conkin.Extra.SymMap
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

import qualified Data.Map as Map
import Data.Singletons (Sing, SomeSing(SomeSing), sing)
import Data.Singletons.Decide ((:~:)(Refl), Decision(..), (%~))
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Conkin.Extra.Some

newtype SymMap (v :: Symbol -> *) = SymMap (Map.Map (SomeSing Symbol) (Some v))

empty :: SymMap v
empty = SymMap Map.empty

singleton :: forall x v . Sing x -> v x -> SymMap v
singleton k v = SymMap (Map.singleton (SomeSing k) (some k v))

union :: SymMap v -> SymMap v -> SymMap v
union (SymMap x) (SymMap y) = SymMap $ Map.union x y

difference, (\\) :: SymMap v -> SymMap v -> SymMap v
difference (SymMap x) (SymMap y) = SymMap $ Map.difference x y
(\\) = difference

insert :: forall x v . Sing x -> v x -> SymMap v -> SymMap v
insert k v (SymMap m) = SymMap $ Map.insert (SomeSing k) (some k v) m

delete :: SSymbol x -> SymMap v -> SymMap v
delete sx (SymMap m) = SymMap $ Map.delete (SomeSing sx) m

lookup :: forall x v . SSymbol x -> SymMap v -> Maybe (v x)
lookup k (SymMap m) =
  (\(Some (v :: v x')) -> case sing @_ @x' %~ k of
      Disproved{} -> error "Bad value in Map"
      Proved Refl -> v
    )
    <$> Map.lookup (SomeSing k) m

(!?) :: forall x v . SymMap v -> SSymbol x -> Maybe (v x)
(!?) = flip lookup

fromList :: forall v . [Some v] -> SymMap v
fromList xs =
  SymMap $ Map.fromList $ map (\(Some (v :: v x)) -> (SomeSing $ sing @_ @x, Some v)) xs

toList :: forall v . SymMap v -> [Some v]
toList (SymMap m) = map snd $ Map.toList m
