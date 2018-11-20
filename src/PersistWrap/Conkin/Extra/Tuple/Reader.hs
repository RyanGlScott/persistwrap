{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}

module PersistWrap.Conkin.Extra.Tuple.Reader
    ( Reader
    , ReaderT
    , askX
    , runReaderT
    ) where

import Conkin (Tuple)
import Data.Functor.Identity (Identity)
import Data.Singletons (Sing, sing)
import Data.Singletons.Decide
import Data.Singletons.Prelude (SList)
import GHC.Stack (HasCallStack)

import PersistWrap.Conkin.Extra.Some
import PersistWrap.Conkin.Extra.Tuple (mapUncheckSing)
import PersistWrap.Stream (StreamT, runStreamT)
import qualified PersistWrap.Stream as Stream

type ReaderT f = StreamT (Some f)
type Reader f = ReaderT f Identity

askX :: (HasCallStack, Monad m) => SDecide k => Sing (x :: k) -> ReaderT f m (f x)
askX s = do
  Some (x :: f x') <- Stream.get
  case sing @_ @x' %~ s of
    Proved Refl -> return x
    Disproved{} -> error "Unexpected type"

runReaderT :: (HasCallStack, Monad m) => SList xs -> ReaderT f m y -> Tuple xs f -> m y
runReaderT sxs act = runStreamT act . mapUncheckSing sxs Some
