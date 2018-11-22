{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.Type where

import Data.List.NonEmpty (NonEmpty)
import Data.Singletons.TH (singletons)

import PersistWrap.Primitives

$(singletons [d|
  data Structure text
    = Primitive PrimName
    | Foreign text
    | UnitType
    | SumType (NonEmpty (text, Structure text))
    | ProductType [(text, Structure text)]
    | ListType (Structure text)
    | MapType (Structure text) (Structure text)
  |])
