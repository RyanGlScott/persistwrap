module PersistWrap.Table.Monad2
    ( Monad2(..)
    ) where

import Control.Concurrent.STM (STM)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Debug.Trace

class Monad m => Monad2 m where
  return2 :: Show x => x -> m x

instance Monad2 m => Monad2 (ReaderT r m) where
  return2 = lift . return2
instance Monad2 m => Monad2 (StateT s m) where
  return2 = lift . return2

instance Monad2 STM where
  return2 x = trace ("STM returning " ++ show x) $ return $ trace ("STM returned " ++ show x) x
