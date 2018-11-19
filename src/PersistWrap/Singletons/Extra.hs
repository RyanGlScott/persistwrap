{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Singletons.Extra where

import Data.Singletons.TH (singletonsOnly)

$(singletonsOnly [d|
  first :: (a -> b) -> (a, c) -> (b, c)
  first fn (x, y) = (fn x, y)
  |])
