{-# LANGUAGE AllowAmbiguousTypes #-}

module PersistWrap.Table.Class where

import Data.Maybe (isJust)
import Data.Promotion.Prelude (Fst, Snd)
import Data.Proxy (Proxy (Proxy))
import Data.Reflection (Reifies, reflect, reify)

import PersistWrap.Table.Column
import PersistWrap.Table.Row

data Entity (table :: Schema -> *) (tab :: (*,Schema))
  = Entity {entityKey :: Key table tab, entityVal :: Row (SchemaOf tab)}

class Monad m => MonadTable (table :: Schema -> *) m | table -> m where
  data Key table :: (*,Schema) -> *
  getEntities :: forall tab . WithinTable table tab => SubRow (SchemaOf tab) -> m [Entity table tab]
  getRow :: forall tab . WithinTable table tab => Key table tab -> m (Maybe (Row (SchemaOf tab)))
  insertRow :: forall tab . WithinTable table tab => Row (SchemaOf tab) -> m (Key table tab)
  -- Returns True if the row was present.
  deleteRow :: forall tab . WithinTable table tab => Key table tab -> m Bool
  stateRow
    :: forall tab b
     . WithinTable table tab
    => Key table tab
    -> (Row (SchemaOf tab) -> (b, Row (SchemaOf tab)))
    -> m (Maybe b)
  modifyRow
    :: forall tab . WithinTable table tab
    => Key table tab -> (Row (SchemaOf tab) -> Row (SchemaOf tab)) -> m Bool
  modifyRow key fn = isJust <$> stateRow key (((), ) . fn)

type WithinTable (table :: Schema -> *) (tab :: (*,Schema))
  = Reifies (Fst tab) (table (Snd tab))

withinTable
  :: forall table xs y
   . table xs
  -> (forall tab' . WithinTable table '(tab',xs) => Proxy '(tab',xs) -> y)
  -> y
withinTable tab cont = reify tab $ \(_ :: Proxy tab') -> cont (Proxy @'(tab',xs))

getTable :: forall tab table . WithinTable table tab => table (SchemaOf tab)
getTable = reflect (Proxy @(Fst tab))
