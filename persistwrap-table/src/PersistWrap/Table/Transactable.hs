module PersistWrap.Table.Transactable
    ( Entity
    , FK
    , ForeignKey
    , Key
    , MonadDML(..)
    , MonadTransactable(..)
    , Table
    , TabRow
    , TabSubRow
    , WithinTable
    , deleteRow
    , entityToForeign
    , foreignToKey
    , getAllEntities
    , getEntities
    , getRow
    , insertRow
    , keyToForeign
    , lookupTable
    , modifyRow
    , stateRow
    ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)
import Data.Function.Pointless ((.:))
import Data.Proxy (Proxy)
import Data.Singletons.TypeLits (SSymbol)

import PersistWrap.Table.Class (MonadTransaction, entityToForeign, foreignToKey, keyToForeign)
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Column (Sing(SSchema))
import PersistWrap.Table.Reflect (SomeTableNamed, getSchemaSing)
import PersistWrap.Table.Row (unrestricted)

class (Monad m, MonadTransaction (BaseTransactionMonad m)) => MonadTransactable (m :: * -> *) where
  type BaseTransactionMonad m :: * -> *
  liftTransaction :: BaseTransactionMonad m x -> m x

instance (MonadTransactable m, Monoid w) => MonadTransactable (WriterT w m) where
  type BaseTransactionMonad (WriterT w m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction
instance MonadTransactable m => MonadTransactable (ReaderT r m) where
  type BaseTransactionMonad (ReaderT r m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction
instance MonadTransactable m => MonadTransactable (StateT s m) where
  type BaseTransactionMonad (StateT s m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction
instance MonadTransactable m => MonadTransactable (ExceptT e m) where
  type BaseTransactionMonad (ExceptT e m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction

type ForeignKey m = Class.ForeignKey (BaseTransactionMonad m)
type Key m = Class.Key (BaseTransactionMonad m)
type Table m = Class.Table (BaseTransactionMonad m)
type TabRow m tab = Class.TabRow (BaseTransactionMonad m) tab
type TabSubRow m tab = Class.TabSubRow (BaseTransactionMonad m) tab
type WithinTable m tab = Class.WithinTable (BaseTransactionMonad m) tab
type Entity m tab = Class.Entity (BaseTransactionMonad m) tab

getEntities
  :: forall m tab
   . (MonadTransactable m, WithinTable m tab)
  => Proxy tab
  -> TabSubRow m tab
  -> m [Entity m tab]
getEntities = liftTransaction .: Class.getEntities
getRow
  :: forall m tab
   . (MonadTransactable m, WithinTable m tab)
  => Key m tab
  -> m (Maybe (TabRow m tab))
getRow = liftTransaction . Class.getRow
insertRow
  :: forall m tab
   . (MonadTransactable m, WithinTable m tab)
  => Proxy tab
  -> TabRow m tab
  -> m (Key m tab)
insertRow = liftTransaction .: Class.insertRow
-- Returns True if the row was present.
deleteRow :: forall m tab . (MonadTransactable m, WithinTable m tab) => Key m tab -> m Bool
deleteRow = liftTransaction . Class.deleteRow
stateRow
  :: forall m tab b
   . (MonadTransactable m, WithinTable m tab)
  => Key m tab
  -> (TabRow m tab -> (b, TabRow m tab))
  -> m (Maybe b)
stateRow = liftTransaction .: Class.stateRow
modifyRow
  :: forall m tab
   . (MonadTransactable m, WithinTable m tab)
  => Key m tab
  -> (TabRow m tab -> TabRow m tab)
  -> m Bool
modifyRow = liftTransaction .: Class.modifyRow
lookupTable
  :: forall m name
   . MonadTransactable m
  => SSymbol name
  -> m (Maybe (SomeTableNamed (Table m) name))
lookupTable = liftTransaction . Class.lookupTable

getAllEntities
  :: forall tab m . (MonadTransactable m, WithinTable m tab) => Proxy tab -> m [Entity m tab]
getAllEntities proxy = case getSchemaSing proxy of
  SSchema _ cols -> getEntities proxy (unrestricted cols)

class MonadTransactable (Transaction m) => MonadDML m where
  type Transaction m :: * -> *
  atomicTransaction :: Transaction m y -> m y

type FK m = ForeignKey (Transaction m)
