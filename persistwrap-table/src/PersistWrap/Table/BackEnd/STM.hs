{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.BackEnd.STM
    ( STMPersist
    , STMTransaction
    , showAllTables
    , withEmptyTables
    , withEmptyTableProxies
    ) where

import PersistWrap.Table.BackEnd.STM.Internal
