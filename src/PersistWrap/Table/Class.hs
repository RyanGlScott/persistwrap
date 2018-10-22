module PersistWrap.Table.Class where

import Data.Maybe (isJust)
import Data.Promotion.Prelude (Fst, Snd)
import Data.Proxy (Proxy (Proxy))
import Data.Reflection (Reifies, reflect, reify)
import Data.Singletons (SingI, sing)
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Conkin.Extra (HEq)
import PersistWrap.Table.Column
import PersistWrap.Table.Row (unrestricted)
import qualified PersistWrap.Table.Row as Row

type TabRow m tab = Row.Row (ForeignKey m) (TabCols tab)
type TabSubRow m tab = Row.SubRow (ForeignKey m) (TabCols tab)

data Entity m (tab :: (*,Schema))
  = Entity {entityKey :: Key m tab, entityVal :: TabRow m tab}

data SomeTableNamed (table :: Schema -> *) (name :: Symbol)
  = forall cols. SomeTableNamed (Sing cols) (table ('Schema name cols))

class (HEq (ForeignKey m), Monad m) => MonadTransaction m where
  data Table m :: Schema -> *
  data Key m :: (*,Schema) -> *
  type ForeignKey m :: Symbol -> *
  getEntities :: forall tab . WithinTable m tab
    => Proxy tab -> TabSubRow m tab -> m [Entity m tab]
  getRow :: forall tab . WithinTable m tab => Key m tab -> m (Maybe (TabRow m tab))
  insertRow :: forall tab . WithinTable m tab => Proxy tab -> TabRow m tab -> m (Key m tab)
  -- Returns True if the row was present.
  deleteRow :: forall tab . WithinTable m tab => Key m tab -> m Bool
  stateRow
    :: forall tab b
     . WithinTable m tab
    => Key m tab
    -> (TabRow m tab -> (b, TabRow m tab))
    -> m (Maybe b)
  modifyRow
    :: forall tab . WithinTable m tab
    => Key m tab -> (TabRow m tab -> TabRow m tab) -> m Bool
  modifyRow key fn = isJust <$> stateRow key (((), ) . fn)
  lookupTable :: forall name . SSymbol name -> m (Maybe (SomeTableNamed (Table m) name))
  keyToForeign :: forall tab . WithinTable m tab => Key m tab -> ForeignKey m (TabName tab)
  foreignToKey
    :: forall tab . WithinTable m tab => Proxy tab -> ForeignKey m (TabName tab) -> Key m tab

class MonadTransaction (Transaction m) => MonadDML m where
  type Transaction m :: * -> *
  atomicTransaction :: Transaction m y -> m y

type WithinTableOf (table :: Schema -> *) tab =
  (SingI (TabSchema tab), Reifies (Fst tab) (table (Snd tab)))
type WithinTable m tab = WithinTableOf (Table m) tab

withinTable
  :: forall table sch y
   . SingI sch
  => table sch
  -> (forall tab' . WithinTableOf table '(tab',sch) => Proxy '(tab',sch) -> y)
  -> y
withinTable tab cont = reify tab $ \(_ :: Proxy tab') -> cont (Proxy @'(tab',sch))

getTable :: forall tab table proxy . WithinTableOf table tab => proxy tab -> table (TabSchema tab)
getTable _ = reflect (Proxy @(Fst tab))

getAllEntities :: forall tab m . (MonadTransaction m, WithinTable m tab) => Proxy tab -> m [Entity m tab]
getAllEntities proxy = getEntities proxy (unrestricted (getSchemaSing proxy))

getSchemaSing
  :: forall tab table proxy . WithinTableOf table tab => proxy tab -> SSchema (TabSchema tab)
getSchemaSing _ = sing
