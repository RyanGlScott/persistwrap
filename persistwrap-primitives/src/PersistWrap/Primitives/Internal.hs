{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Primitives.Internal where

import Data.Singletons.TH
import Test.QuickCheck.Instances ()

-- TODO Add list and map. Careful about the Arbitrary instances.
$(singletons [d|
  data PrimName
    = PrimText
    | PrimByteString
    | PrimInt64
    | PrimDouble
    | PrimRational
    | PrimBool
    | PrimDay
    | PrimTimeOfDay
    | PrimUTCTime
    deriving (Eq, Ord, Show, Bounded, Enum)
  |])
