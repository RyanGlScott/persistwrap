{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Embedding.Class.Embeddable where

import Data.Bifunctor (second)
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.TypeLits
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import PersistWrap.Embedding.Get
import PersistWrap.Embedding.Insert
import PersistWrap.Embedding.Rep
import PersistWrap.Embedding.Schemas
import PersistWrap.Functor.Extra
import PersistWrap.Structure
import PersistWrap.Table

class MonadTransactable m => Embeddable (schemaName :: Symbol) (x :: *) (m :: * -> *) where
  xSchemas :: [Schema Text]
  getXs :: m [(ForeignKey m schemaName, x)]
  getX :: ForeignKey m schemaName -> m (Maybe x)
  insertX :: x -> m (ForeignKey m schemaName)
  deleteX :: ForeignKey m schemaName -> m Bool
  stateX :: ForeignKey m schemaName -> (x -> (b,x)) -> m (Maybe b)
  modifyX :: ForeignKey m schemaName -> (x -> x) -> m Bool
  modifyX key fn = isJust <$> stateX @schemaName key (((), ) . fn)

newtype MRow m cols = MRow {unMRow :: Row (ForeignKey m) cols}

lookupExpectTable
  :: forall schema m . (HasCallStack, MonadTransactable m) => SSchema schema -> m (Table m schema)
lookupExpectTable (SSchema tn cols) = lookupTable tn <&> \case
  Nothing                       -> error $ "Missing table: " ++ Text.unpack (fromSing tn)
  Just (SomeTableNamed cols' t) -> case cols %~ cols' of
    Disproved{} -> error "Mismatched schema"
    Proved Refl -> t

withExpectTable
  :: forall (tabname :: Symbol) (cols :: [(Symbol, Column Symbol)]) (m :: * -> *) (y :: *)
   . (MonadTransactable m, SingI tabname, SingI cols)
  => (  forall (tab :: (*, Schema Symbol))
      . (WithinTable m tab, TabSchema tab ~ ( 'Schema tabname cols))
     => Proxy tab
     -> m y
     )
  -> m y
withExpectTable continuation = do
  t <- lookupExpectTable (sing @_ @( 'Schema tabname cols))
  withinTable t continuation

instance (SingI tabname, SingI cols, MonadTransactable m) => Embeddable tabname (MRow m cols) m where
  xSchemas = [ fromSing $ SSchema (sing @_ @tabname) (sing @_ @cols) ]
  getXs = withExpectTable @tabname @cols $
    fmap (map (\(Entity k v) -> (keyToForeign k, MRow v))) . getAllEntities
  getX k = withExpectTable @tabname @cols $ \proxy ->
    fmap MRow <$> getRow (foreignToKey proxy k)
  insertX (MRow r) = withExpectTable @tabname @cols $ \proxy ->
    keyToForeign <$> insertRow proxy r
  deleteX k = withExpectTable @tabname @cols $ \proxy -> deleteRow (foreignToKey proxy k)
  stateX k op = withExpectTable @tabname @cols
    $ \proxy -> stateRow (foreignToKey proxy k) (second unMRow . op . MRow)
  modifyX k op = withExpectTable @tabname @cols
    $ \proxy -> modifyRow (foreignToKey proxy k) (unMRow . op . MRow)

class (SingI schemaName, SingI structure) => HasRep fk schemaName structure where
  rep :: NamedSchemaRep fk schemaName structure
  entitySchemas :: [Schema Text]
instance (SingI schemaName, SingI structure) => HasRep fk schemaName structure where
  rep = getSchemaRep (sing @_ @schemaName) (sing @_ @structure)
  entitySchemas = uncurry (:) $ repToSchemas $ rep @fk @schemaName @structure

instance (HasRep fk schemaName structure, MonadTransactable m, fk ~ ForeignKey m)
    => Embeddable schemaName (EntityOf fk structure) m where
  xSchemas = entitySchemas @fk @schemaName @structure
  getXs = undefined
  getX = get (rep @fk @schemaName @structure)
  insertX = insert (rep @fk @schemaName @structure)
  deleteX = undefined
  stateX = undefined
  modifyX = undefined

instance {-# OVERLAPPABLE #-}
    (EntityPart fk x, HasRep fk schemaName (StructureOf x), MonadTransactable m, fk ~ ForeignKey m)
    => Embeddable schemaName x m where
  xSchemas = xSchemas @schemaName @(EntityOf fk (StructureOf x)) @m
  getXs = map (second fromEntity) <$> getXs
  getX = fmap (fmap fromEntity) . getX
  insertX = insertX . toEntity
  deleteX = deleteX @_ @(EntityOf fk (StructureOf x))
  stateX = let stateX' = stateX in \k fn -> stateX' k (second toEntity . fn . fromEntity)
  modifyX = let modifyX' = modifyX in \k fn -> modifyX' k (toEntity . fn . fromEntity)
