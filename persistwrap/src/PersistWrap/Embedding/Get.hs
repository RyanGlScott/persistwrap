module PersistWrap.Embedding.Get
    ( get
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Applicative ((<|>))
import Control.Monad (forM, void)
import Control.Monad.Morph (generalize, hoist)
import Control.Monad.Trans (lift)
import Data.Bijection (biTo)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.TypeLits (SSymbol)
import GHC.Stack (HasCallStack)

import Conkin.Extra (htraverse, mapUncheck, noHere)
import Consin
import qualified Consin.Tuple.StreamReader as Tuple
import PersistWrap.Embedding.Columns
import PersistWrap.Embedding.Rep
import PersistWrap.Embedding.Utils
import PersistWrap.Functor.Extra
import PersistWrap.Maybe.Extra (fromJust)
import PersistWrap.Structure
import PersistWrap.Table as Table
import StreamReader (runStreamReaderT, splitStreamReader)
import qualified StreamReader

get
  :: forall schemaName structure m
   . (HasCallStack, MonadTransactable m)
  => NamedSchemaRep (ForeignKey m) schemaName structure
  -> ForeignKey m schemaName
  -> m (Maybe (EntityOf (ForeignKey m) structure))
get (NamedSchemaRep schemaName rep) fk = withSomeTable schemaName $ \cols proxy ->
  getRow (foreignToKey proxy fk) >>= \case
    Nothing -> return Nothing
    Just row ->
      Just <$> runStreamReaderT (getFromRow (some schemaName fk) rep) (mapUncheckSing cols Some row)

type ValueStreamT m = Tuple.StreamReaderT (ValueSnd (ForeignKey m)) m
type ValueStream fk = Tuple.StreamReader (ValueSnd fk)

getFromRow
  :: (HasCallStack, MonadTransactable m)
  => Some (ForeignKey m)
  -> SchemaRep (ForeignKey m) structure
  -> ValueStreamT m (EntityOf (ForeignKey m) structure)
getFromRow selfKey = \case
  AtMostOneColumnSchema col   -> getColumn sLoneColumnName selfKey col
  ProductSchema         cols  -> Product <$> htraverse (getColumnAs selfKey) cols
  SumUnIndexedSchema def cols -> do
    c1 <- splitStreamReader $ hoist generalize $ nonNullCol cols
    case c1 <|> def of
      Just x  -> Sum <$> getIndexed selfKey cols x
      Nothing -> error "All columns null with no default"
  SumIndexedSchema cols@(getNonEmptyTags -> SomeSing names) -> do
    ValueSnd (V (EV (EnumVal v))) <- Tuple.askX $ sTagNamedColumn names
    Sum <$> getIndexed selfKey cols (proxyMatch cols v)

proxyMatch
  :: HasCallStack
  => Tuple (nxs :: [(Symbol, Structure Symbol)]) f
  -> Tagged (ns :: [Symbol]) Proxy
  -> Tagged nxs Proxy
proxyMatch Nil           _             = error "Tuple longer than tag"
proxyMatch (_ `Cons` _ ) (Here  Proxy) = Here Proxy
proxyMatch (_ `Cons` xs) (There other) = There $ proxyMatch xs other

nonNullCol :: Tuple nxs (NamedColumnRep fk) -> ValueStream fk (Maybe (Tagged nxs Proxy))
nonNullCol = \case
  Nil                             -> return Nothing
  NamedColumnRep _ cr `Cons` ncrs -> do
    thisOne <- not <$> isNull cr
    if thisOne
      then do
        skipNullNamedColumns ncrs
        return $ Just $ Here Proxy
      else fmap There <$> nonNullCol ncrs

getIndexed
  :: forall nxs m
   . (HasCallStack, MonadTransactable m)
  => Some (ForeignKey m)
  -> Tuple nxs (NamedColumnRep (ForeignKey m))
  -> Tagged nxs Proxy
  -> ValueStreamT m (Tagged nxs (EntityOfSnd (ForeignKey m)))
getIndexed selfKey = go
  where
    go
      :: forall nxs'
       . Tuple nxs' (NamedColumnRep (ForeignKey m))
      -> Tagged nxs' Proxy
      -> ValueStreamT m (Tagged nxs' (EntityOfSnd (ForeignKey m)))
    go (NamedColumnRep name cr `Cons` ncrs) (Here Proxy) = do
      result <- getColumn name selfKey cr
      hoist generalize $ skipNullNamedColumns ncrs
      return $ Here $ EntityOfSnd result
    go (NamedColumnRep _ cr `Cons` ncrs) (There rest) =
      hoist generalize (skipNullColumn cr) >> There <$> go ncrs rest
    go Nil tag = noHere tag

skipNullColumn :: HasCallStack => ColumnRep fk x -> ValueStream fk ()
skipNullColumn = \case
  UnitRep{}        -> return ()
  FnRep cr _       -> skipNullColumn cr
  PrimRep{}        -> void StreamReader.ask
  ForeignRep{}     -> error "Not nullable"
  NullForeignRep{} -> void StreamReader.ask
  ListRep{}        -> return ()
  MapRep{}         -> return ()

skipNullNamedColumn :: HasCallStack => NamedColumnRep fk x -> ValueStream fk ()
skipNullNamedColumn (NamedColumnRep _ cr) = skipNullColumn cr
skipNullNamedColumns :: HasCallStack => Tuple xs (NamedColumnRep fk) -> ValueStream fk ()
skipNullNamedColumns = sequence_ . mapUncheck skipNullNamedColumn

isNull :: ColumnRep fk x -> ValueStream fk Bool
isNull = \case
  UnitRep{}        -> return True
  PrimRep _        -> checkNullValue
  FnRep cr _       -> isNull cr
  ForeignRep     _ -> checkNullValue
  NullForeignRep _ -> checkNullValue
  ListRep{}        -> return True
  MapRep{}         -> return True

checkNullValue :: ValueStream fk Bool
checkNullValue = StreamReader.ask <&> \case
  Some (ValueSnd (N Nothing)) -> True
  _                           -> False

getColumnAs
  :: (HasCallStack, MonadTransactable m)
  => Some (ForeignKey m)
  -> NamedColumnRep (ForeignKey m) nx
  -> ValueStreamT m (EntityOfSnd (ForeignKey m) nx)
getColumnAs selfKey (NamedColumnRep colname cr) = EntityOfSnd <$> getColumn colname selfKey cr

getColumn
  :: forall x m colName
   . (HasCallStack, MonadTransactable m)
  => SSymbol colName
  -> Some (ForeignKey m)
  -> ColumnRep (ForeignKey m) x
  -> ValueStreamT m x
getColumn colName selfKey = \case
  UnitRep x                               -> return x
  FnRep c fn                              -> biTo fn <$> getColumn colName selfKey c
  PrimRep    c                            -> Tuple.askX (STuple2 colName c) <&> \(ValueSnd v) -> v
  ForeignRep rep@(NamedSchemaRep sname _) -> do
    ValueSnd (V (FKV fk)) <- Tuple.askX (STuple2 colName (SColumn SFalse (SForeignKey sname)))
    lift $ fromJust <$> get rep fk
  NullForeignRep rep@(NamedSchemaRep sname _) -> do
    ValueSnd (N (v :: Maybe (BaseValue (ForeignKey m) ( 'Table.ForeignKey ref)))) <- Tuple.askX
      (STuple2 colName (SColumn STrue (SForeignKey sname)))
    case v of
      Nothing       -> return Nothing
      Just (FKV fk) -> lift $ Just . fromJust <$> get rep fk
  ListRep (NamedSchemaRep tabName x) ->
    List . makeList <$> collectionList (getListItem x) selfKey tabName
  MapRep keyRep (NamedSchemaRep tabName valRep) ->
    Map . makeMap <$> collectionList (getMapItem keyRep valRep) selfKey tabName

collectionList
  :: forall m a tabName
   . (HasCallStack, MonadTransactable m)
  => (Some (ForeignRow (ForeignKey m)) -> m a)
  -> Some (ForeignKey m)
  -> SSymbol tabName
  -> ValueStreamT m [a]
collectionList convertRow (getSome -> GetSome selfSchemaName selfKey) tabName =
  lift $ withSomeTable tabName $ \case
    cn `SCons` restCols -> case cn %~ sContainerNamedColumn selfSchemaName of
      Disproved{} -> error "Subtable has incorrect key column"
      Proved Refl -> \proxy -> do
        entities <- getEntities
          proxy
          (MaybeValueSnd (Just (V (FKV selfKey))) `Cons` unrestricted restCols)
        forM entities $ convertRow . Some . entityToForeign
    SNil -> error "Subtable has no columns"

data ListItem x = ListItem{index :: Int, value :: x}

getListItem
  :: forall m structure
   . (HasCallStack, MonadTransactable m)
  => SchemaRep (ForeignKey m) structure
  -> Some (ForeignRow (ForeignKey m))
  -> m (ListItem (EntityOf (ForeignKey m) structure))
getListItem structRep (getSome -> GetSome (SSchema name cols) (ForeignRow fk r)) =
  (`runStreamReaderT` mapUncheckSing cols Some r) $ do
    _                   <- StreamReader.ask
    ValueSnd (V (PV i)) <- Tuple.askX sIndexNamedColumn
    x                   <- getFromRow (some name fk) structRep
    return $ ListItem (fromIntegral i) x

data MapItem k v = MapItem{key :: k, value:: v}

getMapItem
  :: forall m keystruct valstruct
   . (HasCallStack, MonadTransactable m)
  => ColumnRep (ForeignKey m) (EntityOf (ForeignKey m) keystruct)
  -> SchemaRep (ForeignKey m) valstruct
  -> Some (ForeignRow (ForeignKey m))
  -> m (MapItem (EntityOf (ForeignKey m) keystruct) (EntityOf (ForeignKey m) valstruct))
getMapItem keyrep valrep (getSome -> GetSome (SSchema name cols) (ForeignRow fk r)) =
  (`runStreamReaderT` mapUncheckSing cols Some r) $ do
    _ <- StreamReader.ask
    k <- getColumn sKeyColumnName (some name fk) keyrep
    v <- getFromRow (some name fk) valrep
    return $ MapItem k v

makeList :: [ListItem x] -> [x]
makeList = map (\ListItem { value } -> value) . sortOn index

makeMap :: Ord k => [MapItem k v] -> Map.Map k v
makeMap = Map.fromList . map (\MapItem {..} -> (key, value))
