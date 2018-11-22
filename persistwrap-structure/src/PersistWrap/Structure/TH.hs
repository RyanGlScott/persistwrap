{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.TH
    ( deriveEntityPart
    , module X
    ) where

import GHC.Generics (Rep)
import Language.Haskell.TH (Dec, Q, Type)

import Consin.TH as X
import PersistWrap.Structure.EntityPart
import PersistWrap.Structure.Orphans ()

deriveEntityPart :: Q Type -> Q [Dec]
deriveEntityPart mt = do
  t <- mt
  [d|
    instance EntityPart fk $(return t) where
      type StructureOf $(return t) = GStructureOf (Rep $(return t))
    |]
