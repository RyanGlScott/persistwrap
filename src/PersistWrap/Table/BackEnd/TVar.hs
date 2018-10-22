module PersistWrap.Table.BackEnd.TVar ( TVarDMLT, STMTransaction, withEmptyTables ) where

import Control.Arrow ((&&&), (***))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (MonadReader, ReaderT, asks, mapReaderT, runReaderT)
import Data.Constraint (Dict (Dict))
import Data.List (group, sort)
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Singletons (SomeSing (SomeSing), fromSing, sing, withSingI)
import Data.Singletons.Decide ((:~:) (..), Decision (..), (%~))
import Data.Singletons.TypeLits (withKnownSymbol)
import qualified Data.Singletons.TypeLits as S (SSymbol)
import Data.Text (Text)
import Unsafe.Coerce (unsafeCoerce)

import PersistWrap.Conkin.Extra ((:*:) ((:*:)), HEq (..), HOrd (..), Some (Some))
import PersistWrap.Conkin.Extra.Map (Map)
import qualified PersistWrap.Conkin.Extra.Map as Map
import PersistWrap.Functor.Extra ((<&>))
import PersistWrap.Table.Class
    ( Entity (..)
    , Key
    , MonadDML
    , MonadTransaction (..)
    , SomeTableNamed (SomeTableNamed)
    , getSchemaSing
    , getTable
    )
import qualified PersistWrap.Table.Class as Class
import PersistWrap.Table.Column
import PersistWrap.Table.Row

type TVarMaybeRow xs = TVar (Maybe (Row FK xs))

newtype SSymbol name = SSymbol (S.SSymbol name)
instance HEq SSymbol where
  heq (SSymbol x) (SSymbol y) = case x %~ y of
    Proved dec -> Just $ case dec of {Refl -> Dict}
    Disproved _ -> Nothing
instance HOrd SSymbol where
  hcompare (SSymbol x) (SSymbol y) = compare (fromSing x) (fromSing y)

type TableMap s = Map SSymbol (SomeTableNamed (Table (STMTransaction s)))

newtype STMTransaction s x = STMTransaction (ReaderT (TableMap s) STM x)
  deriving (Functor, Applicative, Monad, MonadBase STM, MonadReader (TableMap s))

data FK name = forall sch. SchemaName sch ~ name => FK (SSchema sch) (TVarMaybeRow (SchemaCols sch))

instance HEq FK where
  heq (FK sl l) (FK sr r) = case sl %~ sr of
    Proved Refl -> if l == r then Just Dict else Nothing
    _ -> Nothing

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
      SSchema _ scols -> withSingI scols $ filter (matches restriction . entityVal) result
  getRow (Key r) = liftBase $ readTVar r
  insertRow proxy rowValues = liftBase $ do
    let Table refs = getTable proxy
    newRowKey <- newTVar (Just rowValues)
    modifyTVar refs (newRowKey :)
    return $ Key newRowKey
  deleteRow (Key r) = liftBase $ stateTVar r $ isJust &&& const Nothing
  stateRow (Key r) fn = liftBase $ stateTVar r $ maybe (Nothing, Nothing) ((Just *** Just) . fn)
  lookupTable sname = withKnownSymbol sname $ asks $ Map.lookup (SSymbol sname)
  keyToForeign (Key r :: Key (STMTransaction s) tab) = FK (sing :: SSchema (TabSchema tab)) r
  foreignToKey (_ :: Proxy tab) (FK (_ :: SSchema sch) r) = return $ Key $ coerceSchema r
    where
      -- If the names are the same, then the schemas must be the same.
      coerceSchema
        :: forall. SchemaName sch ~ TabName tab
        => TVarMaybeRow (SchemaCols sch) -> TVarMaybeRow (TabCols tab)
      coerceSchema = unsafeCoerce

newtype TVarDMLT s m x = TVarDMLT (ReaderT (TableMap s) m x)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadDML (TVarDMLT s m) where
  type Transaction (TVarDMLT s m) = STMTransaction s
  atomicTransaction (STMTransaction act) = TVarDMLT $ mapReaderT (liftIO . atomically) act

withEmptyTables :: MonadIO m => [SomeSing Schema] -> (forall s . TVarDMLT s m x) -> m x
withEmptyTables schemas (TVarDMLT action)
  | anyDuplicates (map schemaName schemas) = error "Schema names are not distinct"
  | otherwise                              = runReaderT action =<< liftIO (constructMap schemas)

anyDuplicates :: Ord x => [x] -> Bool
anyDuplicates = any (\grp -> length grp > 1) . group . sort

schemaName :: SomeSing Schema -> Text
schemaName (SomeSing (SSchema n _)) = fromSing n

constructMap :: [SomeSing Schema] -> IO (TableMap s)
constructMap = fmap Map.fromList . mapM newTable

newTable :: SomeSing Schema -> IO (Some (SSymbol :*: SomeTableNamed (Table (STMTransaction s))))
newTable (SomeSing (SSchema name cols)) = newTVarIO [] <&> \tabContents ->
  withSingI name $ Some $ SSymbol name :*: SomeTableNamed cols (Table tabContents)
