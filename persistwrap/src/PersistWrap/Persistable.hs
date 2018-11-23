{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Persistable where

import Data.Bifunctor (second)
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
