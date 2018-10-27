{-# LANGUAGE TemplateHaskell #-}

import Conkin (Tuple (..))
import Control.Monad (forM_)
import Control.Monad.State (execStateT)
import qualified Control.Monad.State as State
import qualified Data.Aeson as JSON
import Data.List (find)
import Test.Hspec

import PersistWrap.Conkin.Extra.Tuple.TH (tuple)
import qualified PersistWrap.Table.BackEnd.TVar as BackEnd
import PersistWrap.Table hiding (Enum, JSON)
import PersistWrap.Table.TH

removeInd :: Int -> [a] -> [a]
removeInd i xs = take i xs ++ drop (i + 1) xs

sameElements :: forall a . Eq a => [a] -> [a] -> Bool
sameElements xs ys = maybe False null $ (`execStateT` ys) $ forM_ xs $ \x -> do
  ys'    <- State.get
  (i, _) <- State.lift $ find ((== x) . snd) (zip [0 ..] ys')
  State.put $ removeInd i ys'

shouldBeIgnoreOrder :: (HasCallStack, Eq a, Show a) => [a] -> [a] -> Expectation
shouldBeIgnoreOrder x y = (x, y) `shouldSatisfy` uncurry sameElements

main :: IO ()
main = hspec $ describe "Tables" $ it "should do row operations" $ do
  assertions :: Expectation <- BackEnd.withEmptyTableProxies $(tuple [|
        [ $(schema "tab1" ["abc" ::: Nullable Int64])
        , $(schema "tab2" [])
        , $(schema "tab3"
              [ "hello" ::: Bool
              , "there" ::: Enum ["a", "b", "Belgium"]
              ,  "world" ::: JSON
              ])
        , $(schema "connection" ["key1" ::: Key "tab1", "key3" ::: Nullable (Key "tab3")])
        ]
      |])
    $ \(STP t1 `Cons` STP t2 `Cons` STP t3 `Cons` STP t4 `Cons` Nil) -> do
        (fk1, fk3, assertions) <- atomicTransaction $ do
          k1 <- insertRow t1 $(row [| [10] |])
          let fk1 = keyToForeign k1
          _ <- insertRow t1 $(row [| [null] |])
          k2 <- insertRow t2 Nil
          _ <- insertRow t2 $(row [| [] |])
          deleted1 <- deleteRow k2
          deleted2 <- deleteRow k2
          let assertion1 = do
                deleted1 `shouldBe` True
                deleted2 `shouldBe` False
          k3 <- insertRow t3 $(row [| [False, enum @"a", JSON.String "jsontext"] |])
          _ <- insertRow t3 $(row [| [True, enum @"Belgium", JSON.String "anotherstring"] |])
          let fk3 = keyToForeign k3
          conk <- insertRow t4 $(row [| [fk1, null] |])
          _ <- insertRow t4 $(row [| [fk1, null] |])
          modified <- modifyRow conk $ const $(row [| [fk1, fk3] |])
          let assertion2 = modified `shouldBe` True
          return (fk1, fk3, assertion1 >> assertion2)
        (t1Rows, t2Rows, t3Rows, t3False, t4Rows) <- atomicTransaction $ do
          t1Rows <- getAllEntities t1
          t2Rows <- getAllEntities t2
          t3Rows <- getAllEntities t3
          t3False <- getEntities t3 $(matcher [| [False, any, any] |])
          t4Rows <- getAllEntities t4
          return
            ( map entityVal t1Rows
            , map entityVal t2Rows
            , map entityVal t3Rows
            , map entityVal t3False
            , map entityVal t4Rows
            )
        return $ do
          assertions
          t1Rows `shouldBeIgnoreOrder` [$(row [| [10] |]), $(row [| [null] |])]
          t2Rows `shouldBeIgnoreOrder` [Nil]
          t3Rows `shouldBeIgnoreOrder`
            [ $(row [| [False, enum @"a", JSON.String "jsontext"] |])
            , $(row [| [True, enum @"Belgium", JSON.String "anotherstring"] |])
            ]
          t3False `shouldBeIgnoreOrder` [$(row [| [False, enum @"a", JSON.String "jsontext"] |])]
          t4Rows `shouldBeIgnoreOrder` [$(row [| [fk1, fk3] |]), $(row [| [fk1, null] |])]
  assertions
