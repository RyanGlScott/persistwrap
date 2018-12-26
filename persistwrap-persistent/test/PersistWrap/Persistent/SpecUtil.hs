module PersistWrap.Persistent.SpecUtil
    ( shouldBeIgnoreOrder
    ) where

import Test.Hspec

import Control.Monad (forM_)
import Control.Monad.State (execStateT)
import qualified Control.Monad.State as State
import Data.List (find)
import GHC.Stack (HasCallStack)

removeInd :: Int -> [a] -> [a]
removeInd i xs = let (xsbefore, xsafter) = splitAt i xs in xsbefore ++ tail xsafter

sameElements :: forall a . Eq a => [a] -> [a] -> Bool
sameElements xs ys = maybe False null $ (`execStateT` ys) $ forM_ xs $ \x -> do
  ys'    <- State.get
  (i, _) <- State.lift $ find ((== x) . snd) (zip [0 ..] ys')
  State.put $ removeInd i ys'

shouldBeIgnoreOrder :: (HasCallStack, Eq a, Show a) => [a] -> [a] -> Expectation
shouldBeIgnoreOrder x y = (x, y) `shouldSatisfy` uncurry sameElements
