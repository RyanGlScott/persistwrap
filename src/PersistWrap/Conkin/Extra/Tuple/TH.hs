{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Conkin.Extra.Tuple.TH where

import Conkin (Tuple(Cons, Nil))
import Language.Haskell.TH (Exp(ListE), Q)

tuple :: Q Exp -> Q Exp
tuple = (=<<) $ \case
  ListE exps -> do
    let go = \case
          []     -> [| Nil |]
          x : xs -> [| $(return x) `Cons` $(go xs) |]
    go exps
  e -> error $ "Not a list expression: " ++ show e
