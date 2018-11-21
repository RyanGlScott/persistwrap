module PersistWrap.Table.Class where

import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Singletons (sing)
import Data.Singletons.TypeLits (SSymbol, Symbol)

import PersistWrap.Conkin.Extra (AlwaysS, HEq)
import PersistWrap.Table.Column
import PersistWrap.Table.Reflect
import PersistWrap.Table.Row (ForeignRow(..))
import qualified PersistWrap.Table.Row as Row

type TabRow m tab = Row.Row (ForeignKey m) (TabCols tab)
type TabSubRow m tab = Row.SubRow (ForeignKey m) (TabCols tab)

type Entity m tab = Entity' (Key m tab) (TabRow m tab)

class (HEq (ForeignKey m), AlwaysS Eq (ForeignKey m), AlwaysS Ord (ForeignKey m), Monad m)
    => MonadTransaction m where
  data Table m :: Schema Symbol -> *
  data Key m :: (*,Schema Symbol) -> *
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

type WithinTable m tab = WithinTableOf (Table m) tab

entityToForeign
  :: forall tab m schema
   . (MonadTransaction m, WithinTable m tab, schema ~ TabSchema tab)
  => Entity m tab
  -> ForeignRow (ForeignKey m) schema
entityToForeign (Entity k r) = case sing @_ @schema of
  SSchema _ _ -> ForeignRow (keyToForeign k) r
