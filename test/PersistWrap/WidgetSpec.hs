module PersistWrap.WidgetSpec
    ( spec
    ) where

import Control.Monad (join)
import qualified Data.ByteString.Char8 as BS
import Data.Singletons
import Test.Hspec

import PersistWrap.Embedding.Class.Embeddable (entitySchemas)
import PersistWrap.Embedding.Class.Embedded
import PersistWrap.Structure
import PersistWrap.Table
import qualified PersistWrap.Table.BackEnd.TVar as BackEnd

import Widget

spec :: Spec
spec = describe "Widget" $ it "should get back what you put in" $ do
  let widgetSchemas = entitySchemas @BackEnd.FK @"widget" @(StructureOf (Widget BackEnd.FK))
      intSchemas    = entitySchemas @BackEnd.FK @"abc" @(StructureOf Int)
  case toSing (widgetSchemas ++ intSchemas) of
    (SomeSing sSchemas) -> join $ BackEnd.withEmptyTables sSchemas $ \_ ->
      runItemized @'[ '("abc", Int), '("widget", Widget BackEnd.FK)] $ atomicTransaction $ do
        fk3 <- insertX @"abc" 3
        let
          w1 = Blarg (False, True, BS.pack "hello world")
          w2 = Glorp fk3
          w3 =
            Foo2
              [ Foo { bar = 10, baz = A 3 4, qux = Just Green }
              , Foo { bar = 11, baz = B, qux = Nothing }
              ]
        fkw1      <- insertX @"widget" w1
        fkw2      <- insertX @"widget" w2
        fkw3      <- insertX @"widget" w3
        resultABC <- getX fk3
        result1   <- getX fkw1
        result2   <- getX fkw2
        result3   <- getX fkw3
        return $ do
          resultABC `shouldBe` Just 3
          result1 `shouldBe` Just w1
          result2 `shouldBe` Just w2
          result3 `shouldBe` Just w3
