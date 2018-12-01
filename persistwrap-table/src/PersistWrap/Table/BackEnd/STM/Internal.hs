{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.BackEnd.STM.Internal
    ( STMPersist
    , STMTransaction
    , showAllTables
    , unsafeSetupEmptyTables
    , withEmptyTables
    , withEmptyTableProxies
    ) where


import Conkin (Tuple)
import Control.Arrow ((&&&), (***))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Monad (forM)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, mapReaderT, runReaderT)
import Data.Constraint (Dict(Dict))
import Data.List (group, sort)
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Singletons (SingI, fromSing, sing, withSingI)
import Data.Singletons.Decide ((:~:)(..), Decision(..), (%~))
import Data.Singletons.Prelude hiding (All, Map)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import Conkin.Extra (htraverse)
import Consin hiding (Functor(..))
import Consin.SingMap (SingMap)
import qualified Consin.SingMap as SingMap
import PersistWrap.Table.Class (Entity, MonadBaseTransaction)
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Schema
import PersistWrap.Table.Reflect
import PersistWrap.Table.Row
import PersistWrap.Table.STM.Future (stateTVar)
import PersistWrap.Table.Transaction (ForeignKey, MonadPersist, MonadTransaction, getAllEntities)
import qualified PersistWrap.Table.Transaction as Transaction

type TVarMaybeRow s xs = TVar (Maybe (Row (FK s) xs))

type TableMap s = SingMap (SomeTableNamed (Table s))

newtype STMTransaction s x = STMTransaction (ReaderT (TableMap s) STM x)
  deriving (Functor, Applicative, Monad, MonadBase STM, MonadReader (TableMap s))

data FK s name = forall sch . SchemaName sch ~ name
  => FK (SSchema sch) (TVarMaybeRow s (SchemaCols sch))

instance Show (FK s name) where
  show (FK (SSchema schname _) _) = "<foreign key: " ++ Text.unpack (fromSing schname) ++ ">"
instance AlwaysS Show (FK s) where withAlwaysS = const id

instance Eq (FK s name) where
  (==) (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> l == r
    Disproved{} -> False
instance AlwaysS Eq (FK s) where withAlwaysS = const id

instance Ord (FK s name) where
  -- TODO Figure this one out.
  compare _ _ = error "TVars not comparable"
instance AlwaysS Ord (FK s) where withAlwaysS = const id

type Key s = Class.Key (STMTransaction s)
type Table s = Class.Table (STMTransaction s)

type instance ForeignKey (STMTransaction s) = FK s

instance MonadBaseTransaction (STMTransaction s) where
  newtype Table (STMTransaction s) schema = Table (TVar [TVarMaybeRow s (SchemaCols schema)])
  newtype Key (STMTransaction s) tab = Key {unKey :: TVarMaybeRow s (TabCols tab)}
  getEntities (proxy :: Proxy tab) restriction = liftBase $ do
    let Table refs = getTable proxy
    (result :: [Entity (STMTransaction s) tab])
      <- mapMaybeM (\k -> fmap (Entity (Key k)) <$> readTVar k) =<< readTVar refs
    writeTVar refs $ map (unKey . entityKey) result
    return $ case getSchemaSing proxy of
      SSchema _ (singInstance -> SingInstance) -> filter (matches restriction . entityVal) result
  getRow (Key r) = liftBase $ readTVar r
  insertRow proxy rowValues = liftBase $ do
    let Table refs = getTable proxy
    newRowKey <- newTVar (Just rowValues)
    modifyTVar refs (newRowKey :)
    return $ Key newRowKey
  deleteRow (Key r) = liftBase $ stateTVar r $ isJust &&& const Nothing
  stateRow (Key r) fn = liftBase $ stateTVar r $ maybe (Nothing, Nothing) ((Just *** Just) . fn)
  lookupTable = asks . SingMap.lookup
  keyToForeign (Key r :: Key s tab) = FK (sing @_ @(TabSchema tab)) r
  foreignToKey (_ :: Proxy tab) (FK (SSchema _ schCols :: SSchema sch) r) = Key $ coerceSchema r
    where
      -- If the names are the same, then the schemas must be the same.
      coerceSchema :: forall. TVarMaybeRow s (SchemaCols sch) -> TVarMaybeRow s (TabCols tab)
      coerceSchema = case sing @_ @(TabSchema tab) of
        SSchema _ tabCols -> case schCols %~ tabCols of
          Proved Refl -> id
          Disproved{} -> case Dict :: Dict (SchemaName sch ~ TabName tab) of
            Dict -> error "Two tables with the same name and different schemas"

instance MonadTransaction (STMTransaction s) where
  type BaseTransactionMonad (STMTransaction s) = STMTransaction s
  liftTransaction = id

newtype STMPersist s m x = STMPersist {unSTMPersist :: ReaderT (TableMap s) m x}
  deriving (Functor, Applicative, Monad, MonadIO)

type instance ForeignKey (STMPersist s m) = FK s

instance MonadIO m => MonadPersist (STMPersist s m) where
  type Transaction (STMPersist s m) = STMTransaction s
  atomicTransaction (STMTransaction act) = STMPersist $ mapReaderT (liftIO . atomically) act

withEmptyTables
  :: MonadIO m
  => SList (schemas :: [Schema Symbol])
  -> (forall s . SingI schemas => Tuple schemas (Table s) -> STMPersist s m x)
  -> m x
withEmptyTables = unsafeSetupEmptyTables

unsafeSetupEmptyTables
  :: (HasCallStack, MonadIO m)
  => SList (schemas :: [Schema Symbol])
  -> (SingI schemas => Tuple schemas (Table s) -> STMPersist s m x)
  -> m x
unsafeSetupEmptyTables sschemas action
  | anyDuplicates (map (\(Schema name _) -> name) (fromSing sschemas))
  = error $ "Schema names are not distinct: " ++ show (fromSing sschemas)
  | otherwise
  = do
    tables <- liftIO $ htraverse newTable schemasTuple
    runReaderT (unSTMPersist $ withSingI sschemas action tables) (constructMap sschemas tables)
  where schemasTuple = singToTuple sschemas

withEmptyTableProxies
  :: MonadIO m
  => SList schemas
  -> (forall s . Tuple schemas (SomeTableProxy (Table s)) -> STMPersist s m x)
  -> m x
withEmptyTableProxies schemas action = withEmptyTables schemas (`withinTables` action)

anyDuplicates :: Ord x => [x] -> Bool
anyDuplicates = any (\grp -> length grp > 1) . group . sort

constructMap :: SList schemas -> Tuple schemas (Table s) -> TableMap s
constructMap schemas = SingMap.fromList . mapUncheckSing schemas tableToMapEntry

tableToMapEntry
  :: forall s schema . SingI schema => Table s schema -> Some (SomeTableNamed (Table s))
tableToMapEntry = case sing @_ @schema of
  SSchema name cols -> some name . SomeTableNamed cols

newTable :: proxy schema -> IO (Table s schema)
newTable _ = Table <$> newTVarIO []

showAllTables :: forall s . STMTransaction s String
showAllTables = do
  tm       <- ask
  tabLines <-
    forM (SingMap.toList tm)
      $ \(getSome -> GetSome name (SomeTableNamed (singInstance -> SingInstance) tab)) ->
          withSingI name $ do
            rows <- withinTable tab $ fmap (map (\(Entity _ v) -> v)) . getAllEntities
            return $ Text.unpack (fromSing name) ++ ": " ++ show (map ConsinC rows)
  return $ unlines tabLines
