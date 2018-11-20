{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Embedding.Class.Embedded where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans(..))
import Data.Function.Pointless ((.:))
import Data.Promotion.Prelude (type (==))
import Data.Singletons.TypeLits (Symbol)

import PersistWrap.Embedding.Class.Embeddable (Embeddable)
import qualified PersistWrap.Embedding.Class.Embeddable as E
import PersistWrap.Table (ForeignKey, MonadDML(..), MonadTransactable(..))

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

newtype Itemized (items :: [(Symbol, *)]) m x = Itemized {runItemized :: m x}
  deriving (Functor, Applicative, Monad, MonadTransactable, MonadIO)
instance MonadTrans (Itemized items) where
  lift = Itemized
instance MonadDML m => MonadDML (Itemized items m) where
  type Transaction (Itemized items m) = Itemized items (Transaction m)
  atomicTransaction (Itemized act) = Itemized $ atomicTransaction act

instance Embeddable schemaName x m => Embeddable schemaName x (Itemized items m) where
  xSchemas = E.xSchemas @schemaName @x @m
  getXs = Itemized E.getXs
  getX = Itemized . E.getX
  insertX = Itemized . E.insertX
  deleteX = Itemized . E.deleteX @_ @x
  stateX = Itemized .: E.stateX
  modifyX = Itemized .: E.modifyX

instance (Embeddable schemaName x m, MapsTo schemaName x items)
  => Embedded schemaName x (Itemized items m)

class MapsTo (schemaName :: Symbol) x (items :: [(Symbol, *)]) | schemaName items -> x
instance MapsToH (schemaName == headName) schemaName x ('(headName, headX) ': rest)
  => MapsTo schemaName x ('(headName, headX) ': rest)
class MapsToH (current :: Bool) (schemaName :: Symbol) x (items :: [(Symbol, *)])
  | schemaName items -> x
instance MapsTo schemaName x rest => MapsToH 'False schemaName x (head ': rest)
instance x ~ headX => MapsToH 'True schemaName x ('(headName, headX) ': rest)
