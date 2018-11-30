{-# LANGUAGE TypeInType #-}

module Consin.Tuple.StreamReader
    ( StreamReader
    , StreamReaderT
    , askX
    , runStreamReaderT
    ) where

import Conkin (Tuple)
import Data.Functor.Identity (Identity)
import Data.Singletons (Sing, sing)
import Data.Singletons.Decide
import Data.Singletons.Prelude (SList)
import GHC.Stack (HasCallStack)

import Consin.Some
import Consin.Tuple (mapUncheckSing)
import qualified StreamReader as List

type StreamReaderT f = List.StreamReaderT (Some f)
type StreamReader f = StreamReaderT f Identity

askX :: (HasCallStack, Monad m) => SDecide k => Sing (x :: k) -> StreamReaderT f m (f x)
askX s = do
  Some (x :: f x') <- List.ask
  case sing @_ @x' %~ s of
    Proved Refl -> return x
    Disproved{} -> error "Unexpected type"

runStreamReaderT :: (HasCallStack, Monad m) => SList xs -> StreamReaderT f m y -> Tuple xs f -> m y
runStreamReaderT sxs act = List.runStreamReaderT act . mapUncheckSing sxs Some
