{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module PersistWrap.WidgetSpec
    ( spec_widget
    ) where

import Control.Monad (join)
import Data.Singletons (sing)
import GHC.TypeLits (Symbol)
import Test.Hspec

import Conkin.Extra (Always, withAlways)
import Consin (withAlwaysS)
import PersistWrap
import PersistWrap.BackEnd.Helper (AllEmbed)
import qualified PersistWrap.BackEnd.STM.Itemized as BackEnd
import PersistWrap.Table.Monad2 (Monad2)
import PersistWrap.TestUtils.Widget

-- | We're declaring a new table context which called \"TestTables\".
data TestTables (fk :: Symbol -> *)
instance Always AllEmbed TestTables where
  withAlways = const id
-- |
-- \"TestTables\" has two primary tables in it: \"abc\" and \"widget\". \"abc\" stores `Int`s and
-- \"widget\" stores `Widget`s.
type instance Items (TestTables fk) = '[ '("abc", Int), '("widget", Widget fk)]

spec_widget :: Spec
spec_widget = describe "Widget" $ it "should get back what you put in" $ do
  -- Initializes empty tables in `STM` backend.
  () <- join $ BackEnd.withEmptyTablesItemized @TestTables widgetAssertions
  pure ()

-- |
-- We can declare what the _entire_ persistence layer looks like by wrapping the monad we're working
-- in with @ Itemized TestTables @. For a more fine-grained approach, see `insertWidget3`.
widgetAssertions
  :: forall m
   . (MonadPersist m, ForeignKeysShowable m, Monad2 (Transaction m))
  => ItemizedIn TestTables m Expectation
widgetAssertions = withAlwaysS @Show @(ForeignKey m) (sing @_ @"abc") $ atomicTransaction $ do
  -- The compiler knows 3 is an `Int` because we're inserting it into the \"abc\" table.
  -- @ fk3 :: FK m "abc" @
  _  <- insertX @"abc" 3
  () <- error "After"
  _  <- insertWidget3
  undefined

-- |
-- We can specify a subset of tables in the persistence layer on which we'll be operating using
-- `Persisted` constraints. In this example we claim that the monad we're working
-- in has a table named "widget" which contains @ Widget fk @ \'s.
insertWidget3 :: Persisted "widget" (Widget fk) m => m (ForeignKey m "widget")
insertWidget3 = undefined
