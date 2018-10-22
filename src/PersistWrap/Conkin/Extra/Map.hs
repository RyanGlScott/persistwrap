{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra.Map
    ( Map
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
import Data.Singletons (SingI)
import Unsafe.Coerce (unsafeCoerce)

import PersistWrap.Conkin.Extra.FTuple
import PersistWrap.Conkin.Extra.Some

newtype Map k v = Map (Map.Map (Some k) (Some v))

empty :: Map k v
empty = Map Map.empty

singleton :: SingI x => k x -> v x -> Map k v
singleton k v = Map (Map.singleton (Some k) (Some v))

union :: HOrd k => Map k v -> Map k v -> Map k v
union (Map x) (Map y) = Map $ Map.union x y

difference, (\\) :: HOrd k => Map k v -> Map k v -> Map k v
difference (Map x) (Map y) = Map $ Map.difference x y
(\\) = difference

insert :: (HOrd k, SingI x) => k x -> v x -> Map k v -> Map k v
insert k v (Map m) = Map $ Map.insert (Some k) (Some v) m

delete :: (HOrd k, SingI x) => k x -> Map k v -> Map k v
delete k (Map m) = Map $ Map.delete (Some k) m

-- Undefined behavior if HOrd does not agree with HEq
lookup :: forall x k v . (HOrd k, SingI x) => k x -> Map k v -> Maybe (v x)
lookup k (Map m) =
  (\(Some (v :: v x')) -> (unsafeCoerce :: v x' -> v x) v) <$> Map.lookup (Some k) m

(!?) :: forall x k v . (HOrd k, SingI x) => Map k v -> k x -> Maybe (v x)
(!?) = flip lookup

fromList :: HOrd k => [Some (k :*: v)] -> Map k v
fromList xs = Map $ Map.fromList $ map (\(Some (k :*: v)) -> (Some k, Some v)) xs

toList :: Map k v -> [Some (k :*: v)]
toList (Map m) =
  map (\(Some (k :: k kx), Some (v :: v vx)) -> Some (k :*: (unsafeCoerce :: v vx -> v kx) v))
    $ Map.toList m
