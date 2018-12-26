{-# LANGUAGE AllowAmbiguousTypes #-}

module PersistWrap.BackEnd.Persistent.Itemized
    ( withEmptyTablesItemized
    ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import Conkin.Extra (Always)
import PersistWrap.BackEnd.Helper (AllEmbed, Items, setupHelper)
import PersistWrap.Itemized (Itemized)
import PersistWrap.Table (ForeignKey)
import PersistWrap.BackEnd.Persistent.Internal (PersistentT, unsafeSetupEmptyTables)

withEmptyTablesItemized
  :: forall fnitems m x
   . (HasCallStack, MonadUnliftIO m, MonadLogger m, Always AllEmbed fnitems)
  => Text
  -> Int
  -> (  forall s
      . Itemized (Items (fnitems (ForeignKey (PersistentT s m)))) (PersistentT s m) x
     )
  -> m x
withEmptyTablesItemized dbfile numConnections =
  setupHelper @fnitems (\schs act -> unsafeSetupEmptyTables dbfile numConnections schs (const act))
