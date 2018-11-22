{-# OPTIONS_GHC -Wno-orphans #-}

module PersistWrap.Table.Aeson.Orphans () where

import Data.Aeson (Value(..))

deriving instance Ord Value
