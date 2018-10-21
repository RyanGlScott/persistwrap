{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Singletons.Extra where

import Data.Maybe (fromMaybe)
import Data.Singletons.Prelude.Maybe
import Data.Singletons.TH (singletons)

$(singletons [d|
  firstFromMaybe :: a -> (Maybe a, b) -> (a, b)
  firstFromMaybe def (x, y) = (fromMaybe def x, y)
  |])
