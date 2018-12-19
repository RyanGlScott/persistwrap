module PersistWrap.Persistable.Insert.Utils
    ( InsertT
    , getSumTag
    , nextWrite
    , tellX
    , withPerformInsert
    , withPerformReplace
    , writeNullNamed
    ) where

import Conkin (Tagged(..))
import Control.Monad (replicateM_)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List.NonEmpty (SNonEmpty, Sing((:%|)))
import Data.Singletons.TypeLits
import GHC.Stack (HasCallStack)

import Consin (AlwaysS)
import qualified Consin.Tuple.StreamWriter as Tuple
import PersistWrap.Persistable.Rep
import PersistWrap.Persistable.Utils
import PersistWrap.Structure as Structure
import PersistWrap.Table
import PersistWrap.Table.Monad2 (Monad2, return2)

getSumTag
  :: forall name names xs f
   . SNonEmpty (name ':| names)
  -> Tagged (xs :: [(Symbol, Structure Symbol)]) f
  -> EnumVal (name ': names)
getSumTag (_ :%| names0) = EnumVal . \case
  Here  _    -> Here Proxy
  There tag1 -> There $ go names0 tag1
  where
    go :: forall (names' :: [Symbol]) xs' . SList names' -> Tagged xs' f -> Tagged names' Proxy
    go SNil              _         = error "Disparate lengths"
    go (_ `SCons` _    ) (Here  _) = Here Proxy
    go (_ `SCons` names) (There x) = There $ go names x

writeNullNamed
  :: (HasCallStack, Monad2 m, AlwaysS Show fk)
  => NamedColumnRep fk x
  -> InsertT selfSchemaName cols fk m ()
writeNullNamed (NamedColumnRep _ cr) = writeNull cr

writeNull
  :: (HasCallStack, Monad2 m, AlwaysS Show fk)
  => ColumnRep fk x
  -> InsertT selfSchemaName cols fk m ()
writeNull = \case
  UnitRep{}                               -> return ()
  PrimRep c@(SColumn STrue  _)            -> tellX c (N Nothing)
  PrimRep (  SColumn SFalse _)            -> error "Non-nullable column"
  FnRep cr _                              -> writeNull cr
  ForeignRep{}                            -> error "Non-nullable column"
  NullForeignRep (NamedSchemaRep fname _) -> tellX (SColumn STrue (SForeignKey fname)) (N Nothing)
  ListRep{}                               -> return ()
  MapRep{}                                -> return ()

newtype InsertT selfSchemaName cols fk m x =
    InsertT (
        WriterT (NextOperation selfSchemaName fk m ()) (Tuple.StreamWriterT cols (ValueSnd fk) m) x
      )
  deriving (Functor, Applicative, Monad)
instance MonadTrans (InsertT selfSchemaName cols fk) where
  lift = InsertT . lift . lift

newtype NextOperation (selfSchemaName :: Symbol) fk m x =
    NextOperation (ReaderT (fk selfSchemaName) m x)
  deriving (Functor, Applicative, Monad)

withPerformInsert
  :: forall tabName m
   . (MonadTransaction m)
  => SSymbol tabName
  -> (forall cols . InsertT tabName cols (ForeignKey m) m ())
  -> m (ForeignKey m tabName)
withPerformInsert tabName act = withSomeTable tabName (go act)
  where
    go
      :: forall tab
       . (TabName tab ~ tabName, WithinTable m tab)
      => InsertT tabName (TabCols tab) (ForeignKey m) m ()
      -> SList (TabCols tab)
      -> Proxy tab
      -> m (ForeignKey m tabName)
    go (InsertT act') scols proxy = do
      (NextOperation nextOp, row :: TabRow m tab) <- Tuple.runStreamWriterT (execWriterT act')
                                                                            scols
      fk <- keyToForeign <$> insertRow proxy row
      runReaderT nextOp fk
      return fk

withPerformReplace
  :: forall tabName m
   . (MonadTransaction m)
  => SSymbol tabName
  -> ForeignKey m tabName
  -> (forall cols . InsertT tabName cols (ForeignKey m) m ())
  -> m ()
withPerformReplace tabName fk act = withSomeTable tabName (go act)
  where
    go
      :: forall tab
       . (TabName tab ~ tabName, WithinTable m tab)
      => InsertT tabName (TabCols tab) (ForeignKey m) m ()
      -> SList (TabCols tab)
      -> Proxy tab
      -> m ()
    go (InsertT act') scols proxy = do
      (NextOperation nextOp, row :: TabRow m tab) <- Tuple.runStreamWriterT (execWriterT act')
                                                                            scols
      modifyRow (foreignToKey proxy fk) (const row) >>= \case
        False -> error "Row missing"
        True  -> pure ()
      runReaderT nextOp fk

instance Monad m => Semigroup (NextOperation selfSchemaName fk m ()) where
  (<>)    = (>>)
  sconcat = sequence_
  stimes  = replicateM_ . fromIntegral
instance Monad m => Monoid (NextOperation selfSchemaName fk m ()) where
  mempty  = pure ()
  mappend = (>>)
  mconcat = sequence_

tellX
  :: (HasCallStack, Monad2 m, AlwaysS Show fk)
  => SColumn col
  -> Value fk col
  -> InsertT selfSchemaName cols fk m ()
tellX cn x = InsertT $ lift $ Tuple.tell $ \stup@(STuple2 _ cn') -> case cn' %~ cn of
  Proved Refl -> do
    let v = ValueSnd x
    withSingI stup $ return2 v
  Disproved{} -> error "Column types don't match"

nextWrite :: Monad m => (fk selfSchemaName -> m ()) -> InsertT selfSchemaName cols fk m ()
nextWrite act = InsertT $ Writer.tell (NextOperation (ReaderT act))
