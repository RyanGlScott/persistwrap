module PersistWrap.BackEnd.Persistent
    ( PersistentT
    , SqlQuery
    , runPersistentT
    ) where

import Conkin (Tagged(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Maybe (mapMaybe, isJust)
import Database.Persist ((==.))
import qualified Database.Persist as BackEnd
import Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as BackEnd
import Data.Pool (Pool)
import Data.Proxy (Proxy)
import Data.Singletons
import Data.Singletons.Prelude (Sing(STuple2))

import Conkin.Extra (Index, htraverse, tagCases)
import Consin (IndexedItemS(..), getWithIndexS)
import Consin.SingMap (SingMap)
import qualified Consin.SingMap as SingMap
import PersistWrap.BackEnd.Persistent.NamedRow
import PersistWrap.Table
import PersistWrap.Table.Class (MonadBaseTransaction)
import qualified PersistWrap.Table.Class

type TableMap s = SingMap (SomeTableNamed (Table (SqlQuery s)))

newtype SqlQuery s x = SqlQuery (ReaderT SqlBackend (ReaderT (TableMap s) IO) x)
  deriving (Functor, Applicative, Monad)

type instance ForeignKey (SqlQuery s) = PersistentFK s

instance MonadBaseTransaction (SqlQuery s) where
  data Key (SqlQuery s) tab = KeyPersistent (BackEnd.Key (NamedRow s (TabSchema tab)))
  data Table (SqlQuery s) schema = TablePersistent
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
  type BaseTransactionMonad (SqlQuery s) = (SqlQuery s)
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
