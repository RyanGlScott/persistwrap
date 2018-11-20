module PersistWrap.Embedding.Insert
    ( insert
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Monad (forM_, replicateM_, void)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Writer (WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Bijection (biFrom)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import Data.Semigroup (Semigroup(..))
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List.NonEmpty
import Data.Singletons.TypeLits
import GHC.Stack (HasCallStack)

import PersistWrap.Conkin.Extra
import qualified PersistWrap.Conkin.Extra.Tuple.Writer as Tuple
import PersistWrap.Embedding.Columns
import PersistWrap.Embedding.Rep
import PersistWrap.Embedding.Utils
import PersistWrap.Structure as Structure
import PersistWrap.Table

insert
  :: (HasCallStack, MonadTransaction m)
  => NamedSchemaRep (ForeignKey m) selfSchemaName structure
  -> EntityOf (ForeignKey m) structure
  -> m (ForeignKey m selfSchemaName)
insert (NamedSchemaRep selfSchemaName rep) x =
  withPerformInsert selfSchemaName (insertRowItems selfSchemaName rep x)

insertRowItems
  :: forall m selfSchemaName structure cols
   . (HasCallStack, MonadTransaction m)
  => SSymbol selfSchemaName
  -> SchemaRep (ForeignKey m) structure
  -> EntityOf (ForeignKey m) structure
  -> InsertT selfSchemaName cols m ()
insertRowItems selfSchemaName rep x = case rep of
  AtMostOneColumnSchema cr   -> writeColumn selfSchemaName cr x
  ProductSchema         ncrs -> case x of
    Product xs -> sequence_ $ zipUncheck (writeColumnNamed selfSchemaName) ncrs xs
  SumUnIndexedSchema _ ncrs -> case x of
    Sum xs' -> insertSumItem selfSchemaName ncrs xs'
  SumIndexedSchema ncrs@(getNonEmptyTags -> SomeSing tags@(_ :%| _)) -> case x of
    Sum xs' -> do
      tellX (SColumn SFalse (SEnum tags)) (V (EV $ getSumTag tags xs'))
      insertSumItem selfSchemaName ncrs xs'

getSumTag
  :: forall name names xs f
   . SNonEmpty (name ':| names)
  -> Tagged (xs :: [(Symbol, Structure Symbol)]) f
  -> EnumVal (name ': names)
getSumTag (_ :%| names0) tag0 = EnumVal $ case tag0 of
  Here  _    -> Here Proxy
  There tag1 -> There $ go names0 tag1
  where
    go :: forall (names' :: [Symbol]) xs' . SList names' -> Tagged xs' f -> Tagged names' Proxy
    go SNil              _         = error "Disparate lengths"
    go (_ `SCons` _    ) (Here  _) = Here Proxy
    go (_ `SCons` names) (There x) = There $ go names x

insertSumItem
  :: forall xs m selfSchemaName cols
   . (HasCallStack, MonadTransaction m)
  => SSymbol selfSchemaName
  -> Tuple xs (NamedColumnRep (ForeignKey m))
  -> Tagged xs (EntityOfSnd (ForeignKey m))
  -> InsertT selfSchemaName cols m ()
insertSumItem selfSchemaName = go
  where
    go
      :: forall xs'
       . Tuple xs' (NamedColumnRep (ForeignKey m))
      -> Tagged xs' (EntityOfSnd (ForeignKey m))
      -> InsertT selfSchemaName cols m ()
    go (ncr `Cons` rest) (Here x') =
      writeColumnNamed selfSchemaName ncr x' >> sequence_ (mapUncheck writeNullNamed rest)
    go (ncr `Cons` rest) (There x') = writeNullNamed ncr >> go rest x'
    go Nil               x'         = noHere x'

writeNullNamed
  :: (HasCallStack, MonadTransaction m)
  => NamedColumnRep (ForeignKey m) x
  -> InsertT selfSchemaName cols m ()
writeNullNamed (NamedColumnRep _ cr) = writeNull cr

writeNull
  :: (HasCallStack, MonadTransaction m)
  => ColumnRep (ForeignKey m) x
  -> InsertT selfSchemaName cols m ()
writeNull = \case
  UnitRep{}                               -> return ()
  PrimRep c@(SColumn STrue  _)            -> tellX c (N Nothing)
  PrimRep (  SColumn SFalse _)            -> error "Non-nullable column"
  FnRep cr _                              -> writeNull cr
  ForeignRep{}                            -> error "Non-nullable column"
  NullForeignRep (NamedSchemaRep fname _) -> tellX (SColumn STrue (SForeignKey fname)) (N Nothing)
  ListRep{}                               -> return ()
  MapRep{}                                -> return ()

newtype InsertT selfSchemaName cols m x =
    InsertT (
      WriterT
        (NextOperation selfSchemaName m ())
        (Tuple.WriterT cols (ValueSnd (ForeignKey m)) m)
        x
    )
  deriving (Functor, Applicative, Monad)
instance MonadTrans (InsertT selfSchemaName cols) where
  lift = InsertT . lift . lift
newtype NextOperation selfSchemaName m x = NextOperation (ReaderT (ForeignKey m selfSchemaName) m x)
  deriving (Functor, Applicative, Monad)

withPerformInsert
  :: forall tabName m
   . MonadTransaction m
  => SSymbol tabName
  -> (forall cols . InsertT tabName cols m ())
  -> m (ForeignKey m tabName)
withPerformInsert tabName act = withSomeTable tabName (go act)
  where
    go
      :: forall tab
       . (MonadTransaction m, TabName tab ~ tabName, WithinTable m tab)
      => InsertT tabName (TabCols tab) m ()
      -> SList (TabCols tab)
      -> Proxy tab
      -> m (ForeignKey m tabName)
    go (InsertT act') scols proxy = do
      (NextOperation nextOp, row :: TabRow m tab) <- Tuple.runWriterT (execWriterT act') scols
      k <- insertRow proxy row
      let fk = keyToForeign k
      runReaderT nextOp fk
      return fk

instance Monad m => Semigroup (NextOperation selfSchemaName m ()) where
  (<>) = (>>)
  sconcat = sequence_
  stimes = replicateM_ . fromIntegral
instance Monad m => Monoid (NextOperation selfSchemaName m ()) where
  mempty = pure ()
  mappend = (>>)
  mconcat = sequence_

tellX
  :: (HasCallStack, Monad m)
  => SColumn col
  -> Value (ForeignKey m) col
  -> InsertT selfSchemaName cols m ()
tellX cn x = InsertT $ lift $ Tuple.tell $ \(STuple2 _ cn') -> case cn' %~ cn of
  Proved Refl -> return $ ValueSnd x
  Disproved{} -> error "Column types don't match"

nextWrite :: Monad m => (ForeignKey m selfSchemaName -> m ()) -> InsertT selfSchemaName cols m ()
nextWrite act = InsertT $ Writer.tell (NextOperation (ReaderT act))

writeColumnNamed
  :: MonadTransaction m
  => SSymbol selfSchemaName
  -> NamedColumnRep (ForeignKey m) ncr
  -> EntityOfSnd (ForeignKey m) ncr
  -> InsertT selfSchemaName cols m ()
writeColumnNamed selfSchemaName (NamedColumnRep _ cr) (EntityOfSnd x) =
  writeColumn selfSchemaName cr x

writeColumn
  :: MonadTransaction m
  => SSymbol selfSchemaName
  -> ColumnRep (ForeignKey m) x
  -> x
  -> InsertT selfSchemaName cols m ()
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
