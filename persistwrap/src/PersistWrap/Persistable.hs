{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Persistable where

import Control.Arrow ((***))
import Control.Monad (join, void)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy)
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TypeLits
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import PersistWrap.Functor.Extra
import PersistWrap.Persistable.Get
import PersistWrap.Persistable.Insert
import PersistWrap.Persistable.Rep
import PersistWrap.Persistable.Schemas
import PersistWrap.Structure
import PersistWrap.Table

lookupExpectTable
  :: forall schema m . (HasCallStack, MonadTransaction m) => SSchema schema -> m (Table m schema)
lookupExpectTable (SSchema tn cols) = lookupTable tn <&> \case
  Nothing                       -> error $ "Missing table: " ++ Text.unpack (fromSing tn)
  Just (SomeTableNamed cols' t) -> case cols %~ cols' of
    Disproved{} -> error "Mismatched schema"
    Proved Refl -> t

withExpectTable
  :: forall (tabname :: Symbol) (cols :: [(Symbol, Column Symbol)]) (m :: * -> *) (y :: *)
   . (MonadTransaction m, SingI tabname, SingI cols)
  => (  forall (tab :: (*, Schema Symbol))
      . (WithinTable m tab, TabSchema tab ~ ( 'Schema tabname cols))
     => Proxy tab
     -> m y
     )
  -> m y
withExpectTable continuation = do
  t <- lookupExpectTable (sing @_ @( 'Schema tabname cols))
  withinTable t continuation

class (SingI schemaName, SingI structure) => HasRep schemaName structure where
  rep :: NamedSchemaRep fk schemaName structure
  entitySchemas :: [Schema Text]
instance (SingI schemaName, SingI structure) => HasRep schemaName structure where
  rep = getSchemaRep (sing @_ @schemaName) (sing @_ @structure)
  entitySchemas = uncurry (:) $ repToSchemas $ rep @schemaName @structure

class MonadTransaction m => Persistable (schemaName :: Symbol) (x :: *) (m :: * -> *) where
  getXs :: m [(ForeignKey m schemaName, x)]
  getX :: ForeignKey m schemaName -> m (Maybe x)
  insertX :: x -> m (ForeignKey m schemaName)
  deleteX :: ForeignKey m schemaName -> m Bool
  stateX :: ForeignKey m schemaName -> (x -> (b,x)) -> m (Maybe b)
  modifyX :: ForeignKey m schemaName -> (x -> x) -> m Bool
  -- TODO Entity-aware implementations
  getKV :: (x ~ Map k v, Ord k) => ForeignKey m schemaName -> k -> m (Maybe v)
  getKV fk k = runMaybeT $ do
    m <- MaybeT $ getX fk
    MaybeT $ pure $ Map.lookup k m
  insertKV :: (x ~ Map k v, Ord k) => ForeignKey m schemaName -> k -> v -> m ()
  insertKV fk k v = void $ modifyX fk (Map.insert k v)
  deleteKV :: (x ~ Map k v, Ord k) => ForeignKey m schemaName -> k -> m Bool
  deleteKV fk k =
    fromMaybe False <$> stateX @schemaName @x fk (\m -> (k `Map.member` m, Map.delete k m))
  stateKV :: (x ~ Map k v, Ord k) => ForeignKey m schemaName -> k -> (v -> (b, v)) -> m (Maybe b)
  stateKV fk k op = join <$> stateX
    fk
    (\m -> maybe (Nothing, m) ((Just *** (\v' -> Map.insert k v' m)) . op) (Map.lookup k m))
  modifyKV :: (x ~ Map k v, Ord k) => ForeignKey m schemaName -> k -> (v -> v) -> m Bool
  modifyKV fk k op = isJust <$> stateKV fk k (((),) . op)

instance
    (EntityPart fk x, HasRep schemaName (StructureOf x), MonadTransaction m, fk ~ ForeignKey m)
    => Persistable schemaName x m where
  getXs = map (second (fromEntity @fk)) <$> undefined
  getX = fmap (fmap (fromEntity @fk)) . get (rep @schemaName @(StructureOf x))
  insertX = insert (rep @schemaName @(StructureOf x)) . toEntity @fk
  deleteX = undefined
  stateX =
    let stateX' = undefined in \k fn -> stateX' k (second (toEntity @fk) . fn . fromEntity @fk)
  modifyX = let modifyX' = undefined in \k fn -> modifyX' k (toEntity @fk . fn . fromEntity @fk)
