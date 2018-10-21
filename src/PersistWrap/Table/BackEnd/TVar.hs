module PersistWrap.Table.BackEnd.TVar ( Entity, Key, Table, WithinTable ) where

import Control.Arrow ((&&&), (***))
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar
import Control.Monad.Extra (mapMaybeM)
import Data.Maybe (isJust)

import PersistWrap.Table.Class (getTable)
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Column
import PersistWrap.Table.Row

type TVarMaybeRow xs = TVar (Maybe (Row xs))

newtype Table xs = Table (TVar [TVarMaybeRow xs])

type Entity = Class.Entity Table
type Key = Class.Key Table
type WithinTable tab = Class.WithinTable Table tab

instance Class.MonadTable Table STM where
  newtype Key Table tab = Key {unKey :: TVarMaybeRow (SchemaOf tab)}
  getEntities :: forall tab . WithinTable tab => STM [Entity tab]
  getEntities = do
    let Table refs = getTable @tab
    result <- readTVar refs >>= mapMaybeM (\k -> fmap (Class.Entity (Key k)) <$> readTVar k)
    writeTVar refs $ map (\Class.Entity{entityKey} -> unKey entityKey) result
    return result
  getRow (Key r) = readTVar r
  insertRow :: forall tab . WithinTable tab => Row (SchemaOf tab) -> STM (Key tab)
  insertRow rowValues = do
    let Table refs = getTable @tab
    newRowKey <- newTVar (Just rowValues)
    modifyTVar refs (newRowKey :)
    return $ Key newRowKey
  deleteRow (Key r) = stateTVar r $ isJust &&& const Nothing
  stateRow (Key r) fn = stateTVar r $ maybe (Nothing, Nothing) ((Just *** Just) . fn)
