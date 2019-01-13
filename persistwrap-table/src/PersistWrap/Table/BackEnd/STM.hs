{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.BackEnd.STM
    ( STMPersist
    , STMTransaction
    , showAllTables
    , withEmptyTableProxies
    ) where

import PersistWrap.Table.BackEnd.STM.Internal
