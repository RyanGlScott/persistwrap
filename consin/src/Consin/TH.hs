{-# LANGUAGE TemplateHaskell #-}

module Consin.TH
    ( deriveFnEq
    , deriveFnShow
    ) where

import qualified Conkin
import Generics.Deriving.Eq (geqdefault)
import Generics.Deriving.Show (gshowsPrecdefault)
import Language.Haskell.TH (Dec, Q, Type)

import Consin.Class

-- | Derive `Eq` for a continuation kind.
--
-- See "PersistWrap.TestUtils.Widget" for example usage.
deriveFnEq :: Q Type -> Q [Dec]
deriveFnEq t = [d|
  instance AlwaysS Eq f => Eq ($t f) where
    (==) l r = geqdefault (Conkin.fmap FromAlwaysS l) (Conkin.fmap FromAlwaysS r)
  |]

-- | Derive `Show` for a continuation kind.
--
-- See "PersistWrap.TestUtils.Widget" for example usage.
deriveFnShow :: Q Type -> Q [Dec]
deriveFnShow t = [d|
  instance AlwaysS Show f => Show ($t f) where
    showsPrec d x = gshowsPrecdefault d (Conkin.fmap FromAlwaysS x)
  |]
