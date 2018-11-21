{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Conkin.Extra.TH
    ( deriveFnEq
    , deriveFnShow
    ) where

import qualified Conkin
import Language.Haskell.TH (Dec, Q, Type)

import PersistWrap.Conkin.Extra.Class

-- | Derive `Eq` for a continuation kind.
--
-- See "PersistWrap.SpecUtil.Widget" for example usage.
deriveFnEq :: Q Type -> Q [Dec]
deriveFnEq mt = do
  t <- mt
  [d|
    deriving instance AlwaysS Eq f => Eq ($(return t) (FromAlwaysS f))
    instance {-# OVERLAPPABLE #-} AlwaysS Eq f => Eq ($(return t) f) where
      (==) l r = Conkin.fmap FromAlwaysS l == Conkin.fmap FromAlwaysS r
    |]

-- | Derive `Show` for a continuation kind.
--
-- See "PersistWrap.SpecUtil.Widget" for example usage.
deriveFnShow :: Q Type -> Q [Dec]
deriveFnShow mt = do
  t <- mt
  [d|
    deriving instance AlwaysS Show f => Show ($(return t) (FromAlwaysS f))
    instance {-# OVERLAPPABLE #-} AlwaysS Show f => Show ($(return t) f) where
      showsPrec d x = showsPrec d (Conkin.fmap FromAlwaysS x)
    |]
