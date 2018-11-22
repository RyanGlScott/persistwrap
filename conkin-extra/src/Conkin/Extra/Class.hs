{-# LANGUAGE PolyKinds #-}

module Conkin.Extra.Class where

import Data.Constraint (Dict)

class Always c f where
  dict :: Dict (c (f x))
