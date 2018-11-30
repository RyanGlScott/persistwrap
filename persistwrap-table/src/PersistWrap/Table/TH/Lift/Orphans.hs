{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PersistWrap.Table.TH.Lift.Orphans () where

import Data.List.NonEmpty (NonEmpty(..))
import Language.Haskell.TH.Lift

deriving instance Lift x => Lift (NonEmpty x)
