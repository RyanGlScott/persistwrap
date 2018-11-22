{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Embedding.Class.Embeddable where

import Data.Bifunctor (second)
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
  getXs :: m [(ForeignKey m schemaName, x)]
  getX :: ForeignKey m schemaName -> m (Maybe x)
  insertX :: x -> m (ForeignKey m schemaName)
  deleteX :: ForeignKey m schemaName -> m Bool
  stateX :: ForeignKey m schemaName -> (x -> (b,x)) -> m (Maybe b)
  modifyX :: ForeignKey m schemaName -> (x -> x) -> m Bool

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

class (SingI schemaName, SingI structure) => HasRep schemaName structure where
  rep :: NamedSchemaRep fk schemaName structure
  entitySchemas :: [Schema Text]
instance (SingI schemaName, SingI structure) => HasRep schemaName structure where
  rep = getSchemaRep (sing @_ @schemaName) (sing @_ @structure)
  entitySchemas = uncurry (:) $ repToSchemas $ rep @schemaName @structure

instance
    (EntityPart fk x, HasRep schemaName (StructureOf x), MonadTransactable m, fk ~ ForeignKey m)
    => Embeddable schemaName x m where
  getXs = map (second (fromEntity @fk)) <$> undefined
  getX = fmap (fmap (fromEntity @fk)) . get (rep @schemaName @(StructureOf x))
  insertX = insert (rep @schemaName @(StructureOf x)) . toEntity @fk
  deleteX = undefined
  stateX =
    let stateX' = undefined in \k fn -> stateX' k (second (toEntity @fk) . fn . fromEntity @fk)
  modifyX = let modifyX' = undefined in \k fn -> modifyX' k (toEntity @fk . fn . fromEntity @fk)
