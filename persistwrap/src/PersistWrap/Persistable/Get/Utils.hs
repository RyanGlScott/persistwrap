module PersistWrap.Persistable.Get.Utils
    ( ListItem(..)
    , MapItem(..)
    , ValueStreamT
    , collectionList
    , makeList
    , makeMap
    , nonNullCol
    , proxyMatch
    , skipNullColumn
    , skipNullNamedColumns
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Monad (forM, void)
import Control.Monad.Trans (lift)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.TypeLits (SSymbol)
import GHC.Stack (HasCallStack)

import Conkin.Extra (mapUncheck)
import Consin
import qualified Consin.Tuple.StreamReader as Tuple
import PersistWrap.Functor.Extra
import PersistWrap.Persistable.Columns
import PersistWrap.Persistable.Rep
import PersistWrap.Persistable.Utils
import PersistWrap.Structure
import PersistWrap.Table as Table
import qualified StreamReader

type ValueStreamT m = Tuple.StreamReaderT (ValueSnd (ForeignKey m)) m
type ValueStream fk = Tuple.StreamReader (ValueSnd fk)

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

collectionList
  :: forall m a tabName
   . (HasCallStack, MonadTransaction m)
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

data MapItem k v = MapItem{key :: k, value:: v}

makeList :: [ListItem x] -> [x]
makeList = map (\ListItem { value } -> value) . sortOn index

makeMap :: Ord k => [MapItem k v] -> Map.Map k v
makeMap = Map.fromList . map (\MapItem {..} -> (key, value))
