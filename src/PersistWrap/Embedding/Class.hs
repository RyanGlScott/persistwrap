{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Embedding.Class where

import Data.Bifunctor (second)
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Data.Singletons (SingI, SomeSing(SomeSing), fromSing, sing, toSing)
import Data.Singletons.Decide ((:~:)(Refl), Decision(..), (%~))
import Data.Singletons.TypeLits
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import PersistWrap.Embedding.Rep
import PersistWrap.Embedding.Schemas
import PersistWrap.Functor.Extra ((<&>))
import PersistWrap.Structure
import PersistWrap.Table

class MonadTransaction m => Embeddable (schemaName :: Symbol) (x :: *) (m :: * -> *) where
  xSchemas :: SomeSing [Schema Symbol]
  getXs :: m [(ForeignKey m schemaName, x)]
  getX :: ForeignKey m schemaName -> m (Maybe x)
  insertX :: x -> m (ForeignKey m schemaName)
  deleteX :: ForeignKey m schemaName -> m Bool
  stateX :: ForeignKey m schemaName -> (x -> (b,x)) -> m (Maybe b)
  modifyX :: ForeignKey m schemaName -> (x -> x) -> m Bool
  modifyX key fn = isJust <$> stateX @schemaName key (((), ) . fn)

lookupExpectTable
  :: forall tabname cols m
   . (HasCallStack, SingI tabname, SingI cols, MonadTransaction m)
  => m (Table m ( 'Schema tabname cols))
lookupExpectTable =
  let tn = sing @_ @tabname
  in  lookupTable tn <&> \case
        Nothing                       -> error $ "Missing table: " ++ Text.unpack (fromSing tn)
        Just (SomeTableNamed cols' t) -> case sing @_ @cols %~ cols' of
          Disproved{} -> error "Mismatched schema"
          Proved Refl -> t

withExpectTable
  :: forall tabname cols m y
   . (HasCallStack, SingI tabname, SingI cols, MonadTransaction m)
  => (  forall tab
      . (TabSchema tab ~ 'Schema tabname cols, WithinTable m tab)
     => Proxy tab
     -> m y
     )
  -> m y
withExpectTable continuation = do
  t <- lookupExpectTable @tabname @cols
  withinTable t continuation

newtype MRow m cols = MRow {unMRow :: Row (ForeignKey m) cols}

instance (SingI tabname, SingI cols, MonadTransaction m) => Embeddable tabname (MRow m cols) m where
  xSchemas = SomeSing $ sing @_ @'[ 'Schema tabname cols ]
  getXs = withExpectTable @tabname @cols
    (fmap (map (\(Entity k v) -> (keyToForeign k, MRow v))) . getAllEntities)
  getX k = withExpectTable @tabname @cols $ \proxy -> fmap MRow <$> getRow (foreignToKey proxy k)
  insertX (MRow r) = withExpectTable @tabname @cols $ \proxy -> keyToForeign <$> insertRow proxy r
  deleteX k = withExpectTable @tabname @cols $ \proxy -> deleteRow (foreignToKey proxy k)
  stateX k op = withExpectTable @tabname @cols
    $ \proxy -> stateRow (foreignToKey proxy k) (second unMRow . op . MRow)
  modifyX k op = withExpectTable @tabname @cols
    $ \proxy -> modifyRow (foreignToKey proxy k) (unMRow . op . MRow)

instance (SingI schemaName, SingI structure, MonadTransaction m)
    => Embeddable schemaName (EntityOf structure) m where
  xSchemas =
    toSing $ repToSchemas $ getSchemaRep (fromSing $ sing @_ @schemaName) (sing @_ @structure)
  getXs = undefined
  getX = undefined
  insertX = undefined
  deleteX = undefined
  stateX = undefined
  modifyX = undefined
