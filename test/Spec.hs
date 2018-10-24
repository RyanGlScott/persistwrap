{-# LANGUAGE TemplateHaskell #-}

import Conkin (Tuple(..))
import Control.Monad (join)
import qualified Data.Aeson as JSON
import Test.Hspec

import PersistWrap.Conkin.Extra.Tuple.TH (tuple)
import PersistWrap.Table.Class
import PersistWrap.Table.TH
import qualified PersistWrap.Table.BackEnd.TVar as BackEnd

main :: IO ()
main = hspec $ describe "Tables" $ it "should insert rows" $ do
  () <- join $ BackEnd.withEmptyTableProxies $(tuple [|
        [ $(schema "tab1" ["abc" ::: Nullable Int64])
        , $(schema "tab2" [])
        , $(schema "tab3" ["hello" ::: Bool, "world" ::: JSON])
        , $(schema "connection" ["key1" ::: Key "tab1", "key3" ::: Nullable (Key "tab3")])
        ]
      |])
    $ \(STP t1Proxy `Cons` STP t2Proxy `Cons` STP t3Proxy `Cons` STP conProxy `Cons` Nil) -> do
        (_fk1, _fk3) <- atomicTransaction $ do
          k1 <- insertRow t1Proxy $(row [| [10] |])
          let fk1 = keyToForeign k1
          _ <- insertRow t1Proxy $(row [| [null] |])
          _ <- insertRow t2Proxy $(row [| [] |])
          k3 <- insertRow t3Proxy $(row [| [False, JSON.String "jsontext"] |])
          let fk3 = keyToForeign k3
          _ <- insertRow conProxy $(row [| [fk1, fk3] |])
          _ <- insertRow conProxy $(row [| [fk1, null] |])
          return (fk1, fk3)
        (_t1Rows, _t2Rows, _t3Rows, _t4Rows) <- atomicTransaction $ do
          t1Rows <- getAllEntities t1Proxy
          t2Rows <- getAllEntities t2Proxy
          t3Rows <- getAllEntities t2Proxy
          conRows <- getAllEntities conProxy
          return (t1Rows, t2Rows, t3Rows, conRows)
        return (return () :: IO ()) -- TODO Assert something meaningful
  return () :: IO ()
