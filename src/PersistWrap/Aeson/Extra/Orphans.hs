{-# OPTIONS_GHC -Wno-orphans #-}

module PersistWrap.Aeson.Extra.Orphans () where

import Data.Aeson (Value (..))

deriving instance Ord Value
