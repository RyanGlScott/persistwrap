module PersistWrap.STM.Future
    ( stateTVar
    ) where

import Control.Concurrent.STM (STM, TVar, readTVar, writeTVar)

stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar tvar op = do
  x <- readTVar tvar
  let (res, y) = op x
  writeTVar tvar y
  return res
