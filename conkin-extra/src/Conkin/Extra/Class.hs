{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Conkin.Extra.Class where

class Always c f where
  withAlways :: forall proxy x y. proxy x -> (c (f x) => y) -> y
