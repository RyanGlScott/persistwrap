module PersistWrap.BackEnd.Persistent.Internal where

import Conkin (Tagged(..), Tuple(..))
import qualified Conkin
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.List (inits)
import Data.Maybe (mapMaybe, isJust)
import Database.Persist (EntityDef, (==.))
import qualified Database.Persist as BackEnd
import Database.Persist.Sql (SqlBackend, runSqlPool)
import qualified Database.Persist.Sql as BackEnd
import Database.Persist.Sqlite (withSqlitePool)
import Data.Pool (Pool)
import Data.Proxy (Proxy)
import Data.Singletons
import Data.Singletons.Prelude (SList, Sing(STuple2))
import Data.Singletons.TypeLits (Symbol)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import Conkin.Extra (Index, htraverse, mapUncheck, tagCases)
import Consin (IndexedItemS(..), getWithIndexS, singToTuple)
import qualified Consin.SingMap as SingMap
import PersistWrap.BackEnd.Persistent.NamedRow
import PersistWrap.Table hiding (Table)
import PersistWrap.Table.BackEnd.Helper (checkForDuplicates, constructMap)
import qualified PersistWrap.Table.BackEnd.Helper as Helper
import PersistWrap.Table.Class (MonadBaseTransaction)
import qualified PersistWrap.Table.Class as Class

type Table s = Class.Table (SqlQuery s)
type TableMap s = Helper.TableMap (Table s)

newtype SqlQuery s x = SqlQuery (ReaderT SqlBackend (ReaderT (TableMap s) IO) x)
  deriving (Functor, Applicative, Monad)

type instance ForeignKey (SqlQuery s) = PersistentFK s

instance MonadBaseTransaction (SqlQuery s) where
  data Key (SqlQuery s) tab = KeyPersistent (BackEnd.Key (NamedRow s (TabSchema tab)))
  data Table (SqlQuery s) schema = TablePersistent EntityDef
  getEntities proxy r = do
    results <- SqlQuery (BackEnd.selectList (makeFilters proxy r) [])
    return $ map (\(BackEnd.Entity k (NamedRow v)) -> Entity (KeyPersistent k) v) results
  getRow (KeyPersistent k) = fmap (\(NamedRow r) -> r) <$> SqlQuery (BackEnd.get k)
  insertRow _ row = KeyPersistent <$> SqlQuery (BackEnd.insert (NamedRow row))
  deleteRow k0@(KeyPersistent k) = do
    wasPresent <- isJust <$> getRow k0
    SqlQuery (BackEnd.delete k)
    pure wasPresent
  stateRow k0@(KeyPersistent k) op = getRow k0 >>= \case
    Nothing -> pure Nothing
    Just v  -> do
      let (y, v') = op v
      SqlQuery (BackEnd.replace k (NamedRow v'))
      pure $ Just y
  lookupTable name = SqlQuery $ lift $ asks $ SingMap.lookup name
  keyToForeign (KeyPersistent (NamedRowKey k)) = PersistentFK k
  foreignToKey _ (PersistentFK k) = KeyPersistent $ NamedRowKey k

instance MonadTransaction (SqlQuery s) where
  type BaseTransactionMonad (SqlQuery s) = SqlQuery s
  liftTransaction = id

makeFilters
  :: forall tab s
   . SingI (TabSchema tab)
  => Proxy tab
  -> SubRow (PersistentFK s) (TabCols tab)
  -> [BackEnd.Filter (NamedRow s (TabSchema tab))]
makeFilters _ = case sing @_ @(TabSchema tab) of
  SSchema _ (singInstance -> SingInstance) ->
    mapMaybe (fmap taggedToFilter . htraverse traverseMaybeValueSnd) . tagCases

traverseMaybeValueSnd :: MaybeValueSnd fk nc -> Maybe (ValueSnd fk nc)
traverseMaybeValueSnd (MaybeValueSnd x) = ValueSnd <$> x

taggedToFilter
  :: SingI cols
  => Tagged cols (ValueSnd (PersistentFK s))
  -> BackEnd.Filter (NamedRow s ( 'Schema name cols))
taggedToFilter (getWithIndexS -> IndexedItemS (i :: Index cols nc) (ValueSnd x)) =
  case sing @_ @nc of
    STuple2 _ (singInstance -> SingInstance) -> ColField i ==. x

newtype PersistentT s m a = PersistentT (ReaderT (Pool SqlBackend) (ReaderT (TableMap s) m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runPersistentT :: Pool SqlBackend -> TableMap s -> PersistentT s m a -> m a
runPersistentT pool tableMap (PersistentT act) = runReaderT (runReaderT act pool) tableMap

type instance ForeignKey (PersistentT s m) = PersistentFK s

instance MonadIO m => MonadPersist (PersistentT s m) where
  type Transaction (PersistentT s m) = SqlQuery s
  atomicTransaction (SqlQuery act) = PersistentT $ do
    pool <- ask
    tm   <- lift ask
    liftIO $ runReaderT (BackEnd.runSqlPool act pool) tm

withEmptyTableProxies
  :: (MonadLogger m, MonadUnliftIO m)
  => Text
  -> Int
  -> SList schemas
  -> (forall s . Tuple schemas (SomeTableProxy (Table s)) -> PersistentT s m x)
  -> m x
withEmptyTableProxies dbfile ncons schemas action =
  unsafeSetupEmptyTables dbfile ncons schemas (`withinTables` action)

unsafeSetupEmptyTables
  :: (HasCallStack, MonadLogger m, MonadUnliftIO m)
  => Text
  -> Int
  -> SList (schemas :: [Schema Symbol])
  -> (SingI schemas => Tuple schemas (Table s) -> PersistentT s m x)
  -> m x
unsafeSetupEmptyTables dbfile ncons sschemas action =
  checkForDuplicates sschemas $ withSqlitePool dbfile ncons $ \pool -> do
    let schemasTuple = singToTuple sschemas
        tables       = Conkin.fmap (TablePersistent . namedRowEntity . fromSing) schemasTuple
        m            = makeAllMigrations (mapUncheck (\(TablePersistent ent) -> ent) tables)
    runSqlPool (BackEnd.runMigration m) pool
    runPersistentT pool (constructMap sschemas tables) (withSingI sschemas action tables)

makeAllMigrations :: [EntityDef] -> BackEnd.Migration
makeAllMigrations allDefs = foldl (*>) (pure ()) (zipWith BackEnd.migrate (inits allDefs) allDefs)
