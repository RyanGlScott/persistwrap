{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PersistWrap.Embedding.Class.Embedded
    ( Embedded
    , deleteX
    , getX
    , getXs
    , insertX
    , modifyX
    , stateX
    ) where

import Data.Promotion.Prelude (type (==))
import GHC.TypeLits (KnownSymbol, Symbol)

import PersistWrap.Embedding.Class.Embeddable (Embeddable)
import qualified PersistWrap.Embedding.Class.Embeddable as E
import PersistWrap.Embedding.Class.Itemized
import PersistWrap.Structure (EntityPart)
import PersistWrap.Table (ForeignKey)

class Embeddable schemaName x m => Embedded schemaName x m | schemaName m -> x

getXs :: Embedded schemaName x m => m [(ForeignKey m schemaName, x)]
getXs = E.getXs
getX :: Embedded schemaName x m => ForeignKey m schemaName -> m (Maybe x)
getX = E.getX
insertX :: Embedded schemaName x m => x -> m (ForeignKey m schemaName)
insertX = E.insertX
deleteX :: forall schemaName x m . Embedded schemaName x m => ForeignKey m schemaName -> m Bool
deleteX = E.deleteX @_ @x
stateX :: Embedded schemaName x m => ForeignKey m schemaName -> (x -> (b, x)) -> m (Maybe b)
stateX = E.stateX
modifyX :: Embedded schemaName x m => ForeignKey m schemaName -> (x -> x) -> m Bool
modifyX = E.modifyX

instance
  ( EntityPart (ForeignKey m) x
  , Embeddable schemaName x m
  , KnownSymbol schemaName
  , MapsTo schemaName x items
  ) => Embedded schemaName x (Itemized items m)

class MapsTo (schemaName :: Symbol) x (items :: [(Symbol, *)]) | schemaName items -> x
instance MapsToH (schemaName == headName) schemaName x ('(headName, headX) ': rest)
  => MapsTo schemaName x ('(headName, headX) ': rest)
class MapsToH (current :: Bool) (schemaName :: Symbol) x (items :: [(Symbol, *)])
  | schemaName items -> x
instance MapsTo schemaName x rest => MapsToH 'False schemaName x (head ': rest)
instance x ~ headX => MapsToH 'True schemaName x ('(headName, headX) ': rest)
