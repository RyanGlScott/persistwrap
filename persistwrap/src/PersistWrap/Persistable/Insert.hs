module PersistWrap.Persistable.Insert
    ( insert
    , insertRowItems
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Monad (forM_, void)
import Control.Monad.Trans (lift)
import Data.Bijection (biFrom)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits
import GHC.Stack (HasCallStack)

import Conkin.Extra (mapUncheck, noHere, zipUncheck)
import Consin (AlwaysS)
import PersistWrap.Persistable.Columns
import PersistWrap.Persistable.Insert.Utils
import PersistWrap.Persistable.Rep
import PersistWrap.Persistable.Utils
import PersistWrap.Structure as Structure
import PersistWrap.Table
import PersistWrap.Table.Monad2 (Monad2)

insert
  :: (HasCallStack, MonadTransaction m, Monad2 m, fk ~ ForeignKey m, AlwaysS Show fk)
  => NamedSchemaRep fk selfSchemaName structure
  -> EntityOf fk structure
  -> m (ForeignKey m selfSchemaName)
insert (NamedSchemaRep selfSchemaName rep) x =
  withPerformInsert selfSchemaName (insertRowItems selfSchemaName rep x)

insertRowItems
  :: forall m selfSchemaName structure fk cols
   . (HasCallStack, MonadTransaction m, Monad2 m, fk ~ ForeignKey m, AlwaysS Show fk)
  => SSymbol selfSchemaName
  -> SchemaRep fk structure
  -> EntityOf fk structure
  -> InsertT selfSchemaName cols fk m ()
insertRowItems selfSchemaName = \case
  AtMostOneColumnSchema cr   -> writeColumn selfSchemaName cr
  ProductSchema         ncrs -> \case
    Product xs -> sequence_ $ zipUncheck (writeColumnNamed selfSchemaName) ncrs xs
  SumUnIndexedSchema _ ncrs -> \case
    Sum xs' -> insertSumItem selfSchemaName ncrs xs'
  SumIndexedSchema ncrs@(getNonEmptyTags -> SomeSing tags@(_ :%| _)) -> \case
    Sum xs' -> do
      tellX (SColumn SFalse (SEnum tags)) (V (EV $ getSumTag tags xs'))
      insertSumItem selfSchemaName ncrs xs'

insertSumItem
  :: forall xs m selfSchemaName fk cols
   . (HasCallStack, MonadTransaction m, Monad2 m, fk ~ ForeignKey m, AlwaysS Show fk)
  => SSymbol selfSchemaName
  -> Tuple xs (NamedColumnRep fk)
  -> Tagged xs (EntityOfSnd fk)
  -> InsertT selfSchemaName cols fk m ()
insertSumItem selfSchemaName = go
  where
    go
      :: forall xs'
       . Tuple xs' (NamedColumnRep fk)
      -> Tagged xs' (EntityOfSnd fk)
      -> InsertT selfSchemaName cols fk m ()
    go (ncr `Cons` rest) (Here x') = do
      writeColumnNamed selfSchemaName ncr x'
      sequence_ (mapUncheck writeNullNamed rest)
    go (ncr `Cons` rest) (There x') = do
      writeNullNamed ncr
      go rest x'
    go Nil x' = noHere x'

writeColumnNamed
  :: (MonadTransaction m, Monad2 m, fk ~ ForeignKey m, AlwaysS Show fk)
  => SSymbol selfSchemaName
  -> NamedColumnRep fk ncr
  -> EntityOfSnd fk ncr
  -> InsertT selfSchemaName cols fk m ()
writeColumnNamed selfSchemaName (NamedColumnRep _ cr) (EntityOfSnd x) =
  writeColumn selfSchemaName cr x

writeColumn
  :: (MonadTransaction m, Monad2 m, fk ~ ForeignKey m, AlwaysS Show fk)
  => SSymbol selfSchemaName
  -> ColumnRep fk x
  -> x
  -> InsertT selfSchemaName cols fk m ()
writeColumn selfSchemaName cr x = case cr of
  UnitRep _    -> return ()
  PrimRep col  -> tellX col x
  FnRep col fn -> writeColumn selfSchemaName col (biFrom fn x)
  ForeignRep subRep@(NamedSchemaRep subSchemaName _) -> do
    fk <- lift $ insert subRep x
    tellX (SColumn SFalse (SForeignKey subSchemaName)) (V (FKV fk))
  NullForeignRep subRep@(NamedSchemaRep subSchemaName _) -> do
    fk <- traverse (lift . insert subRep) x
    tellX (SColumn STrue (SForeignKey subSchemaName)) (N (FKV <$> fk))
  ListRep (NamedSchemaRep subSchemaName subRep) -> case x of
    List els -> nextWrite $ \selfFk -> forM_ (zip [(0 :: Int64) ..] els) $ \(i, v) ->
      void $ withPerformInsert subSchemaName $ do
        tellX (sContainerColumn selfSchemaName) (V $ FKV selfFk)
        tellX sIndexColumn                      (V $ PV i)
        insertRowItems subSchemaName subRep v
  MapRep keyRep (NamedSchemaRep subSchemaName valRep) -> case x of
    Map m -> nextWrite $ \selfFk -> forM_ (Map.toList m) $ \(k, v) ->
      void $ withPerformInsert subSchemaName $ do
        tellX (sContainerColumn selfSchemaName) (V $ FKV selfFk)
        writeColumn subSchemaName keyRep k
        insertRowItems subSchemaName valRep v
