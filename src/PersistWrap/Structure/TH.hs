{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.TH
    ( deriveEntityPart
    ) where

import GHC.Generics (Rep)
import Language.Haskell.TH (Dec, Type, Q)

import PersistWrap.Structure.EntityPart

deriveEntityPart :: Q Type -> Q [Dec]
deriveEntityPart mt = do
  t <- mt
  [d|
    instance EntityPart fk $(return t) where
      type StructureOf $(return t) = GStructureOf (Rep $(return t))
    |]
