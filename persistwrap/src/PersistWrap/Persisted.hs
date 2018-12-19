{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PersistWrap.Persisted
    ( MapsTo
    , Persisted
    , deleteKV
    , deleteX
    , getKV
    , getX
    , getXs
    , insertKV
    , insertX
    , modifyKV
    , modifyX
    , stateKV
    , stateX
    ) where

import Data.Map (Map)
import Data.Promotion.Prelude (type (==))
import GHC.TypeLits (KnownSymbol, Symbol)

import Consin (AlwaysS)
import PersistWrap.Itemized
import PersistWrap.Persistable (Persistable)
import qualified PersistWrap.Persistable as E
import PersistWrap.Structure (EntityPart)
import PersistWrap.Table (ForeignKey)

class Persistable schemaName x m => Persisted schemaName x m | schemaName m -> x

getXs :: Persisted schemaName x m => m [(ForeignKey m schemaName, x)]
getXs = E.getXs
getX :: Persisted schemaName x m => ForeignKey m schemaName -> m (Maybe x)
getX = E.getX
insertX :: Persisted schemaName x m => x -> m (ForeignKey m schemaName)
insertX = E.insertX
deleteX :: forall schemaName x m . Persisted schemaName x m => ForeignKey m schemaName -> m Bool
deleteX = E.deleteX @_ @x
stateX :: Persisted schemaName x m => ForeignKey m schemaName -> (x -> (b, x)) -> m (Maybe b)
stateX = E.stateX
modifyX :: Persisted schemaName x m => ForeignKey m schemaName -> (x -> x) -> m Bool
modifyX = E.modifyX
getKV
  :: (Persisted schemaName x m, x ~ Map k v, Ord k) => ForeignKey m schemaName -> k -> m (Maybe v)
getKV = E.getKV
insertKV
  :: (Persisted schemaName x m, x ~ Map k v, Ord k) => ForeignKey m schemaName -> k -> v -> m ()
insertKV = E.insertKV
deleteKV
  :: forall schemaName x m k v
   . (Persisted schemaName x m, x ~ Map k v, Ord k)
  => ForeignKey m schemaName
  -> k
  -> m Bool
deleteKV = E.deleteKV @_ @x
stateKV
  :: (Persisted schemaName x m, x ~ Map k v, Ord k)
  => ForeignKey m schemaName
  -> k
  -> (v -> (b, v))
  -> m (Maybe b)
stateKV = E.stateKV
modifyKV
  :: (Persisted schemaName x m, x ~ Map k v, Ord k)
  => ForeignKey m schemaName
  -> k
  -> (v -> v)
  -> m Bool
modifyKV = E.modifyKV

instance
  ( AlwaysS Show (ForeignKey m)
  , EntityPart (ForeignKey m) x
  , Persistable schemaName x m
  , KnownSymbol schemaName
  , MapsTo schemaName x items
  ) => Persisted schemaName x (Itemized items m)

class MapsTo (schemaName :: Symbol) x (items :: [(Symbol, *)]) | schemaName items -> x
instance MapsToH (schemaName == headName) schemaName x ('(headName, headX) ': rest)
  => MapsTo schemaName x ('(headName, headX) ': rest)
class MapsToH (current :: Bool) (schemaName :: Symbol) x (items :: [(Symbol, *)])
  | schemaName items -> x
instance MapsTo schemaName x rest => MapsToH 'False schemaName x (head ': rest)
instance x ~ headX => MapsToH 'True schemaName x ('(headName, headX) ': rest)
