module PersistWrap.Table.Transaction
    ( Entity
    , ForeignKey
    , Key
    , MonadPersist(..)
    , MonadTransaction(..)
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

import PersistWrap.Table.Class
    (ForeignKey, MonadBaseTransaction, entityToForeign, foreignToKey, keyToForeign)
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Schema (Sing(SSchema))
import PersistWrap.Table.Reflect (SomeTableNamed, getSchemaSing)
import PersistWrap.Table.Row (unrestricted)

class ( Monad m
      , MonadBaseTransaction (BaseTransactionMonad m)
      , ForeignKey m ~ ForeignKey (BaseTransactionMonad m)
      ) => MonadTransaction (m :: * -> *) where
  type BaseTransactionMonad m :: * -> *
  liftTransaction :: BaseTransactionMonad m x -> m x

type instance ForeignKey (WriterT w m) = ForeignKey m
instance (MonadTransaction m, Monoid w) => MonadTransaction (WriterT w m) where
  type BaseTransactionMonad (WriterT w m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction
type instance ForeignKey (ReaderT r m) = ForeignKey m
instance MonadTransaction m => MonadTransaction (ReaderT r m) where
  type BaseTransactionMonad (ReaderT r m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction
type instance ForeignKey (StateT s m) = ForeignKey m
instance MonadTransaction m => MonadTransaction (StateT s m) where
  type BaseTransactionMonad (StateT s m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction
type instance ForeignKey (ExceptT e m) = ForeignKey m
instance MonadTransaction m => MonadTransaction (ExceptT e m) where
  type BaseTransactionMonad (ExceptT e m) = BaseTransactionMonad m
  liftTransaction = lift . liftTransaction

type Key m = Class.Key (BaseTransactionMonad m)
type Table m = Class.Table (BaseTransactionMonad m)
type TabRow m tab = Class.TabRow (BaseTransactionMonad m) tab
type TabSubRow m tab = Class.TabSubRow (BaseTransactionMonad m) tab
type WithinTable m tab = Class.WithinTable (BaseTransactionMonad m) tab
type Entity m tab = Class.Entity (BaseTransactionMonad m) tab

getEntities
  :: forall m tab
   . (MonadTransaction m, WithinTable m tab)
  => Proxy tab
  -> TabSubRow m tab
  -> m [Entity m tab]
getEntities = liftTransaction .: Class.getEntities
getRow
  :: forall m tab . (MonadTransaction m, WithinTable m tab) => Key m tab -> m (Maybe (TabRow m tab))
getRow = liftTransaction . Class.getRow
insertRow
  :: forall m tab
   . (MonadTransaction m, WithinTable m tab)
  => Proxy tab
  -> TabRow m tab
  -> m (Key m tab)
insertRow = liftTransaction .: Class.insertRow
-- Returns True if the row was present.
deleteRow :: forall m tab . (MonadTransaction m, WithinTable m tab) => Key m tab -> m Bool
deleteRow = liftTransaction . Class.deleteRow
stateRow
  :: forall m tab b
   . (MonadTransaction m, WithinTable m tab)
  => Key m tab
  -> (TabRow m tab -> (b, TabRow m tab))
  -> m (Maybe b)
stateRow = liftTransaction .: Class.stateRow
modifyRow
  :: forall m tab
   . (MonadTransaction m, WithinTable m tab)
  => Key m tab
  -> (TabRow m tab -> TabRow m tab)
  -> m Bool
modifyRow = liftTransaction .: Class.modifyRow
lookupTable
  :: forall m name . MonadTransaction m => SSymbol name -> m (Maybe (SomeTableNamed (Table m) name))
lookupTable = liftTransaction . Class.lookupTable

getAllEntities
  :: forall tab m . (MonadTransaction m, WithinTable m tab) => Proxy tab -> m [Entity m tab]
getAllEntities proxy = case getSchemaSing proxy of
  SSchema _ cols -> getEntities proxy (unrestricted cols)

class (MonadTransaction (Transaction m), ForeignKey m ~ ForeignKey (Transaction m))
    => MonadPersist m where
  type Transaction m :: * -> *
  atomicTransaction :: Transaction m y -> m y
