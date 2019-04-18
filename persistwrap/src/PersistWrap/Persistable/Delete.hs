module PersistWrap.Persistable.Delete
    ( delete
    , deleteDescendants
    ) where

import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.Functor (($>))
import Data.Singletons.Prelude
import Data.Singletons.TypeLits (SSymbol)
import GHC.Stack (HasCallStack)

import Conkin.Extra (mapUncheck)
import Consin
import qualified Consin.Tuple.StreamReader as Tuple
import PersistWrap.Persistable.Columns
import PersistWrap.Persistable.Get.Utils
import PersistWrap.Persistable.Rep
import PersistWrap.Persistable.Utils
import PersistWrap.Table as Table
import StreamReader (runStreamReaderT)
import qualified StreamReader

delete
  :: forall schemaName structure m
   . (HasCallStack, MonadTransaction m)
  => NamedSchemaRep (ForeignKey m) schemaName structure
  -> ForeignKey m schemaName
  -> m Bool
delete (NamedSchemaRep schemaName rep) fk = withSomeTable schemaName $ \cols proxy ->
  getRow (foreignToKey proxy fk) >>= \case
    Nothing -> pure False
    Just row ->
      deleteSomeRow (schemaSubDeletion rep) (some (SSchema schemaName cols) (ForeignRow fk row))
        >> pure True

deleteDescendants
  :: (HasCallStack, MonadTransaction m)
  => NamedSchemaRep (ForeignKey m) schemaName structure
  -> ForeignKey m schemaName
  -> m ()
deleteDescendants (NamedSchemaRep schemaName rep) fk = withSomeTable schemaName $ \cols proxy ->
  getRow (foreignToKey proxy fk) >>= \case
    Nothing -> error "No such row"
    Just row ->
      doSubDeletion (schemaSubDeletion rep) (some (SSchema schemaName cols) (ForeignRow fk row))

deleteColumnChildren
  :: forall fk m colName x
   . (HasCallStack, fk ~ ForeignKey m, MonadTransaction m)
  => SSymbol colName
  -> Some fk
  -> ColumnRep fk x
  -> ValueStreamT m ()
deleteColumnChildren selfName selfKey = \case
  UnitRep _                             -> pure ()
  PrimRep _                             -> StreamReader.ask $> ()
  FnRep cr _                            -> deleteColumnChildren selfName selfKey cr
  ForeignRep nschr@(NamedSchemaRep n _) -> do
    x' <- Tuple.askX (STuple2 selfName (SColumn SFalse (SForeignKey n)))
    case x' of
     ValueSnd (V (FKV fk)) ->
      lift (delete nschr fk) >>= \case
       False -> error "Missing entry"
       True  -> pure ()
  NullForeignRep nschr@(NamedSchemaRep n _) -> do
    x' <- Tuple.askX (STuple2 selfName (SColumn STrue (SForeignKey n)))
    case x' of
     ValueSnd (N (mfk :: Maybe (BaseValue fk ( 'Table.ForeignKey schName)))) ->
      case mfk of
       Nothing       -> pure ()
       Just (FKV fk) -> lift (delete nschr fk) >>= \case
         False -> error "Missing entry"
         True  -> pure ()
  ListRep (NamedSchemaRep tabName x) -> void $ collectionList (deleteSomeListRow x) selfKey tabName
  MapRep krep (NamedSchemaRep valName vrep) ->
    void $ collectionList (deleteSomeMapRow krep vrep) selfKey valName

deleteSomeListRow
  :: (HasCallStack, MonadTransaction m, fk ~ ForeignKey m)
  => SchemaRep fk structure
  -> Some (ForeignRow fk)
  -> m ()
deleteSomeListRow rep = deleteSomeRow $ \someFK -> do
  _ <- StreamReader.ask
  _ <- StreamReader.ask
  schemaSubDeletion rep someFK

deleteSomeMapRow
  :: (HasCallStack, MonadTransaction m, fk ~ ForeignKey m)
  => ColumnRep fk keyStructure
  -> SchemaRep fk valStructure
  -> Some (ForeignRow fk)
  -> m ()
deleteSomeMapRow krep vrep = deleteSomeRow $ \someFK -> do
  _ <- StreamReader.ask
  deleteColumnChildren sKeyColumnName someFK krep
  schemaSubDeletion vrep someFK

schemaSubDeletion
  :: (HasCallStack, MonadTransaction m, fk ~ ForeignKey m)
  => SchemaRep fk structure
  -> Some fk
  -> ValueStreamT m ()
schemaSubDeletion rep someFK = case rep of
  AtMostOneColumnSchema cr -> deleteColumnChildren sLoneColumnName someFK cr
  ProductSchema         xs -> sequence_ $ mapUncheck (deleteNamedColumnChildren someFK) xs
  SumUnIndexedSchema _ xs  -> sequence_ $ mapUncheck (deleteNamedColumnChildren someFK) xs
  SumIndexedSchema xs      -> do
    _ <- StreamReader.ask
    sequence_ $ mapUncheck (deleteNamedColumnChildren someFK) xs

deleteSomeRow
  :: forall fk m
   . (HasCallStack, MonadTransaction m, fk ~ ForeignKey m)
  => (Some fk -> ValueStreamT m ())
  -> Some (ForeignRow fk)
  -> m ()
deleteSomeRow op someFR@(getSome -> GetSome (SSchema schemaName _) (ForeignRow fk _)) = do
  doSubDeletion op someFR
  withSomeTable schemaName $ \_ proxy -> deleteRow (foreignToKey proxy fk) >>= \case
    False -> error "Missing entry"
    True  -> pure ()

doSubDeletion
  :: forall fk m
   . (HasCallStack, MonadTransaction m, fk ~ ForeignKey m)
  => (Some fk -> ValueStreamT m ())
  -> Some (ForeignRow fk)
  -> m ()
doSubDeletion op (getSome -> GetSome (SSchema schemaName cols) (ForeignRow fk row)) = do
  let someFK :: Some fk
      someFK = some schemaName fk
  (`runStreamReaderT` mapUncheckSing cols Some row) (op someFK)

deleteNamedColumnChildren
  :: (HasCallStack, MonadTransaction m)
  => Some (ForeignKey m)
  -> NamedColumnRep (ForeignKey m) nx
  -> ValueStreamT m ()
deleteNamedColumnChildren selfKey (NamedColumnRep name r) = deleteColumnChildren name selfKey r
