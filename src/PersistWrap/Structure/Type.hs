{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.Type where

import Data.Singletons.TH (singletons)

import PersistWrap.Structure.Primitives

$(singletons [d|
  data Structure text
    = Primitive PrimName
    | UnitType
    | SumType (text, Structure text) [(text, Structure text)]
    | ProductType [(text, Structure text)]
    | ListType (Structure text)
    | MapType PrimName (Structure text)
  |])
