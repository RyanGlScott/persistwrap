module PersistWrap.Embedding.Class.Embedded where

import Data.Singletons.TypeLits (Symbol)

import PersistWrap.Embedding.Class.Embeddable (Embeddable)
import qualified PersistWrap.Embedding.Class.Embeddable as E
import PersistWrap.Table (ForeignKey, MonadTransactable(..))

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

newtype Itemized (items :: [(Symbol, *)]) m x = Itemized (m x)
  deriving (Functor, Applicative, Monad, MonadTransactable)
