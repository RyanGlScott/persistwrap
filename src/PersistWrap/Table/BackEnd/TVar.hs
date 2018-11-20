module PersistWrap.Table.BackEnd.TVar
    ( FK
    , TVarDMLT
    , STMTransaction
    , showAllTables
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
import Data.Singletons.Prelude hiding (Map)
import qualified Data.Singletons.TypeLits as S (SSymbol)
import Data.Text (Text)
import qualified Data.Text as Text

import PersistWrap.Conkin.Extra
import PersistWrap.Conkin.Extra.SymMap (SymMap)
import qualified PersistWrap.Conkin.Extra.SymMap as SymMap
import PersistWrap.STM.Future
import PersistWrap.Table.Class
    ( Entity(..)
    , Key
    , MonadDML
    , MonadTransaction(..)
    , SomeTableNamed(SomeTableNamed)
    , SomeTableProxy
    , getSchemaSing
    , getTable
    , withinTables
    )
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Column
import PersistWrap.Table.Row

type TVarMaybeRow xs = TVar (Maybe (Row FK xs))

newtype SSymbolCon name = SSymbolCon (S.SSymbol name)
instance HEq SSymbolCon where
  heq (SSymbolCon x) (SSymbolCon y) = case x %~ y of
    Proved dec -> Just $ case dec of {Refl -> Dict}
    Disproved _ -> Nothing
instance HOrd SSymbolCon where
  hcompare (SSymbolCon x) (SSymbolCon y) = compare (fromSing x) (fromSing y)

type TableMap s = SymMap (SomeTableNamed (Table (STMTransaction s)))

newtype STMTransaction s x = STMTransaction (ReaderT (TableMap s) STM x)
  deriving (Functor, Applicative, Monad, MonadBase STM, MonadReader (TableMap s))

data FK name = forall sch. SchemaName sch ~ name => FK (SSchema sch) (TVarMaybeRow (SchemaCols sch))

instance Show (FK name) where
  show (FK (SSchema schname _) _) = "<foreign key: " ++ Text.unpack (fromSing schname) ++ ">"
instance Always Show FK where dict = Dict

instance Eq (FK name) where
  (==) (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> l == r
    Disproved{} -> False
instance Always Eq FK where dict = Dict
instance HEq FK where
  heq (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> if l == r then Just Dict else Nothing
    Disproved{} -> Nothing

instance Ord (FK name) where
  -- TODO Figure this one out.
  compare _ _ = error "TVars not comparable"
instance Always Ord FK where dict = Dict

instance MonadTransaction (STMTransaction s) where
  newtype Table (STMTransaction s) schema = Table (TVar [TVarMaybeRow (SchemaCols schema)])
  newtype Key (STMTransaction s) tab = Key {unKey :: TVarMaybeRow (TabCols tab)}
  type ForeignKey (STMTransaction s) = FK
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
  lookupTable sname = asks $ SymMap.lookup sname
  keyToForeign (Key r :: Key (STMTransaction s) tab) = FK (sing @_ @(TabSchema tab)) r
  foreignToKey (_ :: Proxy tab) (FK (SSchema _ schCols :: SSchema sch) r) = Key $ coerceSchema r
    where
      -- If the names are the same, then the schemas must be the same.
      coerceSchema
        :: forall. SchemaName sch ~ TabName tab
        => TVarMaybeRow (SchemaCols sch) -> TVarMaybeRow (TabCols tab)
      coerceSchema = case sing @_ @(TabSchema tab) of
        SSchema _ tabCols -> case schCols %~ tabCols of
          Proved Refl -> id
          Disproved{} -> error "Two tables with the same name and different schemas"

newtype TVarDMLT s m x = TVarDMLT {unTVarDMLT :: ReaderT (TableMap s) m x}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadDML (TVarDMLT s m) where
  type Transaction (TVarDMLT s m) = STMTransaction s
  atomicTransaction (STMTransaction act) = TVarDMLT $ mapReaderT (liftIO . atomically) act

withEmptyTables
  :: MonadIO m
  => SList (schemas :: [Schema Symbol])
  -> (forall s . Tuple schemas (Table (STMTransaction s)) -> TVarDMLT s m x)
  -> m x
withEmptyTables sschemas action
  | anyDuplicates (mapUncheck schemaName schemasTuple) = error "Schema names are not distinct"
  | otherwise = do
    tables <- liftIO $ htraverse newTable schemasTuple
    runReaderT (unTVarDMLT $ action tables) (constructMap sschemas tables)
  where schemasTuple = singToTuple sschemas

withEmptyTableProxies
  :: MonadIO m
  => SList schemas
  -> (forall s . Tuple schemas (SomeTableProxy (Table (STMTransaction s))) -> TVarDMLT s m x)
  -> m x
withEmptyTableProxies schemas action =
  withEmptyTables schemas $ withSingI schemas (`withinTables` action)

anyDuplicates :: Ord x => [x] -> Bool
anyDuplicates = any (\grp -> length grp > 1) . group . sort

schemaName :: SSchema (schema :: Schema Symbol) -> Text
schemaName (SSchema n _) = fromSing n

constructMap :: SList schemas -> Tuple schemas (Table (STMTransaction s)) -> TableMap s
constructMap schemas tables = SymMap.fromList $ mapUncheckSing schemas tableToMapEntry tables

tableToMapEntry
  :: forall s schema
   . SingI schema
  => Table (STMTransaction s) schema
  -> Some (SomeTableNamed (Table (STMTransaction s)))
tableToMapEntry tab = case sing @_ @schema of
  SSchema name cols -> some name (SomeTableNamed cols tab)

newTable :: proxy schema -> IO (Table (STMTransaction s) schema)
newTable _ = Table <$> newTVarIO []

showAllTables :: STMTransaction s String
showAllTables = do
  tm       <- ask
  tabLines <-
    forM (SymMap.toList tm)
      $ \(getSome -> GetSome name (SomeTableNamed (cols :: SList cols) tab)) ->
          withSingI name $ withSingI cols $ do
            rows <- Class.withinTable tab $ fmap (map (\(Entity _ v) -> v)) . Class.getAllEntities
            return $ Text.unpack (fromSing name) ++ ": " ++ withAlwaysShow @cols @(ValueSnd FK)
              (show rows)
  return $ unlines tabLines
