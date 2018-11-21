{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.TH
    ( deriveEntityPart
    , deriveEntityPartFK
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

deriveEntityPartFK :: Q Type -> Q [Dec]
deriveEntityPartFK mt = do
  t <- mt
  [d|
    instance EntityPart fk ($(return t) fk) where
      type StructureOf ($(return t) fk) = GStructureOf (Rep ($(return t) fk))
    |]
