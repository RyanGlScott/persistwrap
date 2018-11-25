module PersistWrap.Table.Reflect where

import Conkin (Tuple)
import Data.Promotion.Prelude (Fst, Snd)
import Data.Proxy (Proxy(Proxy))
import Data.Reflection (Reifies, reflect, reify)
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.TypeLits (Symbol)

import Consin (AlwaysS(..), showsPrec1)
import qualified Consin.Tuple as Tuple (fmapSing)
import PersistWrap.Table.Column

data Entity' k v = Entity {entityKey :: k, entityVal :: v}

data SomeTableNamed (table :: Schema Symbol -> *) (name :: Symbol)
  = forall cols. SomeTableNamed (Sing cols) (table ('Schema name cols))

instance (AlwaysS Show table, SingI name) => Show (SomeTableNamed table name) where
  showsPrec d (SomeTableNamed cols tab) = withSingI cols $
    showParen (d > 10) $
      showString "SomeTableNamed " . showsPrec 11 cols . showString " " . showsPrec1 11 tab

instance AlwaysS Show table => AlwaysS Show (SomeTableNamed table) where withAlwaysS = id

type WithinTableOf (table :: Schema Symbol -> *) tab =
  (SingI (TabSchema tab), Reifies (Fst tab) (table (Snd tab)))

withinTable
  :: forall table sch y
   . SingI sch
  => table sch
  -> (forall tab' . WithinTableOf table '(tab',sch) => Proxy '(tab',sch) -> y)
  -> y
withinTable tab cont = reify tab $ \(_ :: Proxy tab') -> cont (Proxy @'(tab',sch))

data SomeTableProxy table sch
  = forall tab. (TabSchema tab ~ sch, WithinTableOf table tab) => STP (Proxy tab)

withinTables
  :: forall table schemas y
   . SingI schemas
  => Tuple schemas table
  -> (Tuple schemas (SomeTableProxy table) -> y)
  -> y
withinTables tables cont = cont $ Tuple.fmapSing (`withinTable` STP) tables

getTable :: forall tab table proxy . WithinTableOf table tab => proxy tab -> table (TabSchema tab)
getTable _ = reflect (Proxy @(Fst tab))

getSchemaSing
  :: forall tab table proxy . WithinTableOf table tab => proxy tab -> SSchema (TabSchema tab)
getSchemaSing _ = sing
