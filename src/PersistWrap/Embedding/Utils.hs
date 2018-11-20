module PersistWrap.Embedding.Utils
    ( getTags
    , getNonEmptyTags
    , withSomeTable
    ) where

import Conkin (Tuple(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy)
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List.NonEmpty
import Data.Singletons.TypeLits
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import PersistWrap.Embedding.Rep
import PersistWrap.Table

withSomeTable
  :: forall tabName m y
   . (HasCallStack, MonadTransactable m)
  => SSymbol tabName
  -> (  forall tab
      . (TabName tab ~ tabName, WithinTable m tab)
     => SList (TabCols tab)
     -> Proxy tab
     -> m y
     )
  -> m y
withSomeTable tn continuation = lookupTable tn >>= \case
  Nothing                      -> error $ "Missing table: " ++ Text.unpack (fromSing tn)
  Just (SomeTableNamed cols t) -> withSingI (SSchema tn cols) $ withinTable t (continuation cols)

getNonEmptyTags :: Tuple (nx ': nxs) (NamedColumnRep fk) -> SomeSing (NonEmpty Symbol)
getNonEmptyTags (NamedColumnRep name _ `Cons` rest) = case getTags rest of
  SomeSing names -> SomeSing (name :%| names)

getTags :: Tuple nxs (NamedColumnRep fk) -> SomeSing [Symbol]
getTags = \case
  Nil -> SomeSing SNil
  (NamedColumnRep name _ `Cons` rest) -> case getTags rest of
    SomeSing names -> SomeSing $ name `SCons` names
