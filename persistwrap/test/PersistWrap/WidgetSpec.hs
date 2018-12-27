module PersistWrap.WidgetSpec
    ( spec_widget
    ) where

import Control.Monad (join)
import qualified Data.ByteString.Char8 as BS
import GHC.TypeLits (Symbol)
import Test.Hspec

import PersistWrap
import qualified PersistWrap.BackEnd.STM.Itemized as BackEnd
import PersistWrap.TestUtils.Widget

-- | We're declaring a new table context which called \"TestTables\".
data TestTables (fk :: Symbol -> *)
-- |
-- \"TestTables\" has two primary tables in it: \"abc\" and \"widget\". \"abc\" stores `Int`s and
-- \"widget\" stores `Widget`s.
type instance Items (TestTables fk) = '[ '("abc", Int), '("widget", Widget fk)]

spec_widget :: Spec
spec_widget =
  describe "Widget"
    $ it "should get back what you put in"
    $ join
    -- Initializes empty tables in `STM` backend.
    $ BackEnd.withEmptyTablesItemized @TestTables widgetAssertions

-- |
-- We can declare what the _entire_ persistence layer looks like by wrapping the monad we're working
-- in with @ Itemized TestTables @. For a more fine-grained approach, see `insertWidget3`.
widgetAssertions :: (MonadPersist m, ForeignKeysShowable m) => ItemizedIn TestTables m Expectation
widgetAssertions = atomicTransaction $ do
  -- The compiler knows 3 is an `Int` because we're inserting it into the \"abc\" table.
  -- @ fk3 :: FK m "abc" @
  fk3 <- insertX @"abc" 3
  let
    -- Similarly we don't need to explicitly specify the type-parameter for w1, w2, and w3.
      w1 = Blarg (False, True, BS.pack "hello world")
      w2 = Glorp fk3
  fkw1      <- insertX @"widget" w1
  fkw2      <- insertX @"widget" w2
  fkw3      <- insertWidget3
  resultABC <- getX fk3
  result1   <- getX fkw1
  result2   <- getX fkw2
  result3   <- getX fkw3
  return $ do
    resultABC `shouldBe` Just 3
    result1 `shouldBe` Just w1
    result2 `shouldBe` Just w2
    result3 `shouldBe` Just widget3

widget3 :: Widget fk
widget3 =
  Foo2 [Foo { bar = 10, baz = A 3 4, qux = Just Green }, Foo { bar = 11, baz = B, qux = Nothing }]

-- |
-- We can specify a subset of tables in the persistence layer on which we'll be operating using
-- `Persisted` constraints. In this example we claim that the monad we're working
-- in has a table named "widget" which contains @ Widget fk @ \'s.
insertWidget3 :: Persisted "widget" (Widget fk) m => m (ForeignKey m "widget")
insertWidget3 = insertX @"widget" widget3
