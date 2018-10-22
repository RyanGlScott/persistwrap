module PersistWrap.Table.BackEnd.TVar ( Entity, Key, STMTable, Table, WithinTable ) where

import Control.Arrow ((&&&), (***))
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Data.Constraint (Dict (Dict))
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Singletons (sing, withSingI)
import Data.Singletons.Decide ((:~:) (..), Decision (..), (%~))
import Data.Singletons.TypeLits (symbolVal, withKnownSymbol)
import qualified Data.Singletons.TypeLits as S (SSymbol)
import Unsafe.Coerce (unsafeCoerce)

import PersistWrap.Conkin.Extra (HEq (..), HOrd (..))
import PersistWrap.Conkin.Extra.Map (Map)
import qualified PersistWrap.Conkin.Extra.Map as Map
import PersistWrap.Table.Class (MonadTable, getSchemaSing, getTable)
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Column
import PersistWrap.Table.Row

type TVarMaybeRow xs = TVar (Maybe (Row FK xs))

type Table s = Class.Table (STMTable s)
type Entity s = Class.Entity (STMTable s)
type Key s = Class.Key (STMTable s)
type SomeTableNamed s = Class.SomeTableNamed (STMTable s)
type WithinTable s tab = Class.WithinTable (STMTable s) tab

newtype SSymbol name = SSymbol (S.SSymbol name)
instance HEq SSymbol where
  heq (SSymbol x) (SSymbol y) = case x %~ y of
    Proved dec -> Just $ case dec of {Refl -> Dict}
    Disproved _ -> Nothing
instance HOrd SSymbol where
  hcompare (SSymbol x) (SSymbol y)
    = compare (withKnownSymbol x $ symbolVal x) (withKnownSymbol y $ symbolVal y)

type TableMap s = Map SSymbol (SomeTableNamed s)

newtype STMTable s x = STMTable (ReaderT (TableMap s) STM x)
  deriving (Functor, Applicative, Monad, MonadBase STM, MonadReader (TableMap s))

data FK name = forall sch. SchemaName sch ~ name => FK (Sing sch) (TVarMaybeRow (SchemaCols sch))

instance HEq FK where
  heq (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> if l == r then Just Dict else Nothing
    _ -> Nothing

instance MonadTable (STMTable s) where
  newtype Table (STMTable s) schema = Table (TVar [TVarMaybeRow (SchemaCols schema)])
  newtype Key (STMTable s) tab = Key {unKey :: TVarMaybeRow (TabCols tab)}
  type ForeignKey (STMTable s) = FK
  getEntitiesProxy (proxy :: Proxy tab) restriction = liftBase $ do
    let Table refs = getTable proxy
    (result :: [Entity s tab])
      <- mapMaybeM (\k -> fmap (Class.Entity (Key k)) <$> readTVar k) =<< readTVar refs
    writeTVar refs $ map (\Class.Entity{entityKey} -> unKey entityKey) result
    return $ case getSchemaSing proxy of
      SSchema _ scols -> withSingI scols $
        filter (\Class.Entity{entityVal} -> matches restriction entityVal) result
  getRow (Key r) = liftBase $ readTVar r
  insertRowProxy proxy rowValues = liftBase $ do
    let Table refs = getTable proxy
    newRowKey <- newTVar (Just rowValues)
    modifyTVar refs (newRowKey :)
    return $ Key newRowKey
  deleteRow (Key r) = liftBase $ stateTVar r $ isJust &&& const Nothing
  stateRow (Key r) fn = liftBase $ stateTVar r $ maybe (Nothing, Nothing) ((Just *** Just) . fn)
  lookupTable sname = withKnownSymbol sname $ asks $ Map.lookup (SSymbol sname)
  keyToForeign (Key r :: Key s tab) = FK (sing :: SSchema (TabSchema tab)) r
  foreignToKeyProxy (_ :: Proxy tab) (FK (_ :: SSchema sch) r) =
      return $ Key $ coerceSchema r
    where
      -- If the names are the same, then the schemas must be the same.
      -- TODO Ensure two tables aren't allowed to have the same name.
      coerceSchema
        :: SchemaName sch ~ TabName tab
        => TVarMaybeRow (SchemaCols sch) -> TVarMaybeRow (TabCols tab)
      coerceSchema = unsafeCoerce
