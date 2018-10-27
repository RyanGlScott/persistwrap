module PersistWrap.Table.BackEnd.TVar
    ( TVarDMLT
    , STMTransaction
    , withEmptyTables
    , withEmptyTableProxies
    ) where

import Conkin (Tuple)
import qualified Conkin
import Control.Arrow ((&&&), (***))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, mapReaderT, runReaderT)
import Data.Constraint (Dict (Dict))
import Data.List (group, sort)
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Singletons (SingI, fromSing, sing, withSingI)
import Data.Singletons.Decide ((:~:) (..), Decision (..), (%~))
import Data.Singletons.Prelude hiding (Map)
import qualified Data.Singletons.TypeLits as S (SSymbol)
import Data.Text (Text)

import PersistWrap.Conkin.Extra
    ( (:*:) ((:*:))
    , Always (..)
    , HEq (..)
    , HOrd (..)
    , Some (Some)
    , htraverse
    , mapUncheck
    , mapUncheckSing
    , tupleToSing
    )
import PersistWrap.Conkin.Extra.Map (Map)
import qualified PersistWrap.Conkin.Extra.Map as Map
import PersistWrap.Table.Class
    ( Entity (..)
    , Key
    , MonadDML
    , MonadTransaction (..)
    , SomeTableNamed (SomeTableNamed)
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

type TableMap s = Map SSymbolCon (SomeTableNamed (Table (STMTransaction s)))

newtype STMTransaction s x = STMTransaction (ReaderT (TableMap s) STM x)
  deriving (Functor, Applicative, Monad, MonadBase STM, MonadReader (TableMap s))

data FK name = forall sch. SchemaName sch ~ name => FK (SSchema sch) (TVarMaybeRow (SchemaCols sch))

instance Eq (FK name) where
  (==) (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> l == r
    Disproved{} -> False
instance Always Eq FK where dict = Dict
instance HEq FK where
  heq (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> if l == r then Just Dict else Nothing
    Disproved{} -> Nothing

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
  lookupTable sname = asks $ withSingI sname Map.lookup (SSymbolCon sname)
  keyToForeign (Key r :: Key (STMTransaction s) tab) = FK (sing :: SSchema (TabSchema tab)) r
  foreignToKey (_ :: Proxy tab) (FK (SSchema _ schCols :: SSchema sch) r) = Key $ coerceSchema r
    where
      -- If the names are the same, then the schemas must be the same.
      coerceSchema
        :: forall. SchemaName sch ~ TabName tab
        => TVarMaybeRow (SchemaCols sch) -> TVarMaybeRow (TabCols tab)
      coerceSchema = case sing :: SSchema (TabSchema tab) of
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
  => Tuple schemas SSchemaCon
  -> (forall s . Tuple schemas (Table (STMTransaction s)) -> TVarDMLT s m x)
  -> m x
withEmptyTables schemas action
  | anyDuplicates (mapUncheck schemaName schemas) = error "Schema names are not distinct"
  | otherwise = do
    tables <- liftIO $ htraverse newTable schemas
    runReaderT (unTVarDMLT $ action tables)
               (withSingI (tupleToSing (Conkin.fmap unSSchemaCon schemas)) constructMap tables)

withEmptyTableProxies
  :: (SingI schemas, MonadIO m)
  => Tuple schemas SSchemaCon
  -> (forall s . Tuple schemas (SomeTableProxy (Table (STMTransaction s))) -> TVarDMLT s m x)
  -> m x
withEmptyTableProxies schemas action = withEmptyTables schemas (`withinTables` action)

anyDuplicates :: Ord x => [x] -> Bool
anyDuplicates = any (\grp -> length grp > 1) . group . sort

schemaName :: SSchemaCon schema -> Text
schemaName (SSchemaCon (SSchema n _)) = fromSing n

constructMap :: SingI schemas => Tuple schemas (Table (STMTransaction s)) -> TableMap s
constructMap tables = Map.fromList $ mapUncheckSing tableToMapEntry tables

tableToMapEntry
  :: forall s schema
   . SingI schema
  => Table (STMTransaction s) schema
  -> Some (SSymbolCon :*: SomeTableNamed (Table (STMTransaction s)))
tableToMapEntry tab = case sing :: Sing schema of
  SSchema name cols -> withSingI name Some (SSymbolCon name :*: SomeTableNamed cols tab)

newTable :: proxy schema -> IO (Table (STMTransaction s) schema)
newTable _ = Table <$> newTVarIO []
