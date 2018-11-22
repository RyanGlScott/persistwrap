{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.BackEnd.TVar
    ( FK
    , TVarDMLT
    , STMTransaction
    , showAllTables
    , withEmptyTables
    , withEmptyTablesItemized
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
import qualified Data.Singletons.TypeLits as S (SSymbol)
import qualified Data.Text as Text

import PersistWrap.Conkin.Extra
import PersistWrap.Conkin.Extra.SymMap (SymMap)
import qualified PersistWrap.Conkin.Extra.SymMap as SymMap
import PersistWrap.Embedding.Class.Embedded (Itemized)
import PersistWrap.STM.Future
import PersistWrap.Table.BackEnd.Helper (AllEmbed, Items, setupHelper)
import PersistWrap.Table.Class (Entity, MonadTransaction)
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Column
import PersistWrap.Table.Reflect
import PersistWrap.Table.Row
import PersistWrap.Table.Transactable (MonadDML, MonadTransactable, getAllEntities)
import qualified PersistWrap.Table.Transactable as Transactable

type TVarMaybeRow s xs = TVar (Maybe (Row (FK s) xs))

newtype SSymbolCon name = SSymbolCon (S.SSymbol name)
instance HEq SSymbolCon where
  heq (SSymbolCon x) (SSymbolCon y) = case x %~ y of
    Proved dec -> Just $ case dec of {Refl -> Dict}
    Disproved _ -> Nothing
instance HOrd SSymbolCon where
  hcompare (SSymbolCon x) (SSymbolCon y) = compare (fromSing x) (fromSing y)

type TableMap s = SymMap (SomeTableNamed (Table s))

newtype STMTransaction s x = STMTransaction (ReaderT (TableMap s) STM x)
  deriving (Functor, Applicative, Monad, MonadBase STM, MonadReader (TableMap s))

data FK s name = forall sch . SchemaName sch ~ name
  => FK (SSchema sch) (TVarMaybeRow s (SchemaCols sch))

instance Show (FK s name) where
  show (FK (SSchema schname _) _) = "<foreign key: " ++ Text.unpack (fromSing schname) ++ ">"
instance AlwaysS Show (FK s) where dictS = Dict

instance Eq (FK s name) where
  (==) (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> l == r
    Disproved{} -> False
instance AlwaysS Eq (FK s) where dictS = Dict
instance HEq (FK s) where
  heq (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> if l == r then Just Dict else Nothing
    Disproved{} -> Nothing

instance Ord (FK s name) where
  -- TODO Figure this one out.
  compare _ _ = error "TVars not comparable"
instance AlwaysS Ord (FK s) where dictS = Dict

type Key s = Class.Key (STMTransaction s)
type Table s = Class.Table (STMTransaction s)

instance MonadTransaction (STMTransaction s) where
  newtype Table (STMTransaction s) schema = Table (TVar [TVarMaybeRow s (SchemaCols schema)])
  newtype Key (STMTransaction s) tab = Key {unKey :: TVarMaybeRow s (TabCols tab)}
  type ForeignKey (STMTransaction s) = FK s
  getEntities (proxy :: Proxy tab) restriction = liftBase $ do
    let Table refs = getTable proxy
    (result :: [Entity (STMTransaction s) tab])
      <- mapMaybeM (\k -> fmap (Entity (Key k)) <$> readTVar k) =<< readTVar refs
    writeTVar refs $ map (unKey . entityKey) result
    return $ case getSchemaSing proxy of
      SSchema _ scols ->  filter (withSingI scols matches restriction . entityVal) result
  getRow (Key r) = liftBase $ readTVar r
  insertRow proxy rowValues = liftBase $ do
    let Table refs = getTable proxy
    newRowKey <- newTVar (Just rowValues)
    modifyTVar refs (newRowKey :)
    return $ Key newRowKey
  deleteRow (Key r) = liftBase $ stateTVar r $ isJust &&& const Nothing
  stateRow (Key r) fn = liftBase $ stateTVar r $ maybe (Nothing, Nothing) ((Just *** Just) . fn)
  lookupTable = asks . SymMap.lookup
  keyToForeign (Key r :: Key s tab) = FK (sing @_ @(TabSchema tab)) r
  foreignToKey (_ :: Proxy tab) (FK (SSchema _ schCols :: SSchema sch) r) = Key $ coerceSchema r
    where
      -- If the names are the same, then the schemas must be the same.
      coerceSchema
        :: forall. SchemaName sch ~ TabName tab
        => TVarMaybeRow s (SchemaCols sch) -> TVarMaybeRow s (TabCols tab)
      coerceSchema = case sing @_ @(TabSchema tab) of
        SSchema _ tabCols -> case schCols %~ tabCols of
          Proved Refl -> id
          Disproved{} -> error "Two tables with the same name and different schemas"

instance MonadTransactable (STMTransaction s) where
  type BaseTransactionMonad (STMTransaction s) = STMTransaction s
  liftTransaction = id

newtype TVarDMLT s m x = TVarDMLT {unTVarDMLT :: ReaderT (TableMap s) m x}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadDML (TVarDMLT s m) where
  type Transaction (TVarDMLT s m) = STMTransaction s
  atomicTransaction (STMTransaction act) = TVarDMLT $ mapReaderT (liftIO . atomically) act

withEmptyTables
  :: MonadIO m
  => SList (schemas :: [Schema Symbol])
  -> (forall s . Tuple schemas (Table s) -> TVarDMLT s m x)
  -> m x
withEmptyTables = setupEmptyTables

setupEmptyTables
  :: MonadIO m
  => SList (schemas :: [Schema Symbol])
  -> (Tuple schemas (Table s) -> TVarDMLT s m x)
  -> m x
setupEmptyTables sschemas action
  | anyDuplicates (map (\(Schema name _) -> name) (fromSing sschemas)) = error
    "Schema names are not distinct"
  | otherwise = do
    tables <- liftIO $ htraverse newTable schemasTuple
    runReaderT (unTVarDMLT $ action tables) (constructMap sschemas tables)
  where schemasTuple = singToTuple sschemas

withEmptyTableProxies
  :: MonadIO m
  => SList schemas
  -> (forall s . Tuple schemas (SomeTableProxy (Table s)) -> TVarDMLT s m x)
  -> m x
withEmptyTableProxies schemas action =
  withEmptyTables schemas $ withSingI schemas (`withinTables` action)

anyDuplicates :: Ord x => [x] -> Bool
anyDuplicates = any (\grp -> length grp > 1) . group . sort

constructMap :: SList schemas -> Tuple schemas (Table s) -> TableMap s
constructMap schemas = SymMap.fromList . mapUncheckSing schemas tableToMapEntry

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
    forM (SymMap.toList tm)
      $ \(getSome -> GetSome name (SomeTableNamed (cols :: SList cols) tab)) ->
          withSingI name $ withSingI cols $ do
            rows <- withinTable tab $ fmap (map (\(Entity _ v) -> v)) . getAllEntities
            return $ Text.unpack (fromSing name) ++ ": " ++ withAlwaysSShow @cols @(ValueSnd (FK s))
              (show rows)
  return $ unlines tabLines

withEmptyTablesItemized
  :: forall fnitems m x
   . (MonadIO m, Always AllEmbed fnitems)
  => (forall s . Itemized (Items (fnitems (FK s))) (TVarDMLT s m) x)
  -> m x
withEmptyTablesItemized = setupHelper @fnitems (\schs act -> setupEmptyTables schs (const act))
