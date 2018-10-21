{-# LANGUAGE AllowAmbiguousTypes #-}

module PersistWrap.Table.Class where

import Data.Maybe (isJust)
import Data.Promotion.Prelude (Fst, Snd)
import Data.Proxy (Proxy (Proxy))
import Data.Reflection (Reifies, reflect, reify)

import PersistWrap.Table.Column
import PersistWrap.Table.Row

data Entity (table :: [Column] -> *) (tab :: (*,[Column]))
  = Entity {entityKey :: Key table tab, entityVal :: Row (Schema tab)}

class Monad m => MonadTable (table :: [Column] -> *) m | table -> m where
  data Key table :: (*,[Column]) -> *
  getEntities :: forall tab . WithinTable table tab => m [Entity table tab]
  getRow :: forall tab . WithinTable table tab => Key table tab -> m (Maybe (Row (Schema tab)))
  insertRow :: forall tab . WithinTable table tab => Row (Schema tab) -> m (Key table tab)
  -- Returns True if the row was present.
  deleteRow :: forall tab . WithinTable table tab => Key table tab -> m Bool
  stateRow
    :: forall tab b
     . WithinTable table tab
    => Key table tab
    -> (Row (Schema tab) -> (b, Row (Schema tab)))
    -> m (Maybe b)
  modifyRow
    :: forall tab . WithinTable table tab
    => Key table tab -> (Row (Schema tab) -> Row (Schema tab)) -> m Bool
  modifyRow key fn = isJust <$> stateRow key (((), ) . fn)

type WithinTable (table :: [Column] -> *) (tab :: (*,[Column]))
  = Reifies (Fst tab) (table (Snd tab))

withinTable
  :: forall table xs y
   . table xs
  -> (forall tab' . WithinTable table '(tab',xs) => Proxy '(tab',xs) -> y)
  -> y
withinTable tab cont = reify tab $ \(_ :: Proxy tab') -> cont (Proxy @'(tab',xs))

getTable :: forall tab table . WithinTable table tab => table (Schema tab)
getTable = reflect (Proxy @(Fst tab))
