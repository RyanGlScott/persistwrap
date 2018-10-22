{-# LANGUAGE PolyKinds #-}

module PersistWrap.Conkin.Extra.Some where

import Data.Constraint (Dict)
import Data.Maybe (isJust)
import Data.Singletons (SingI)

data Some f = forall x. SingI x => Some (f x)

class HEq f where
  heq :: (SingI x, SingI y) => f x -> f y -> Maybe (Dict (x ~ y))
  (==^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (==^) x y = isJust $ x `heq` y
  (/=^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (/=^) x y = not $ x ==^ y
class HEq f => HOrd f where
  hcompare :: (SingI x, SingI y) => f x -> f y -> Ordering
  (<^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (<^) x y = hcompare x y < EQ
  (<=^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (<=^) x y = hcompare x y <= EQ
  (>^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (>^) x y = hcompare x y > EQ
  (>=^) :: (SingI x, SingI y) => f x -> f y -> Bool
  (>=^) x y = hcompare x y >= EQ
  min1 :: (SingI x, SingI y) => f x -> f y -> Some f
  min1 x y = if y <^ x then Some y else Some x
  max1 :: (SingI x, SingI y) => f x -> f y -> Some f
  max1 x y = if y >^ x then Some y else Some x

instance HEq f => Eq (Some f) where
  (==) (Some x) (Some y) = x ==^ y
  (/=) (Some x) (Some y) = x /=^ y
instance HOrd f => Ord (Some f) where
  compare (Some x) (Some y) = hcompare x y
  (<) (Some x) (Some y) = x <^ y
  (<=) (Some x) (Some y) = x <=^ y
  (>) (Some x) (Some y) = x >^ y
  (>=) (Some x) (Some y) = x >=^ y
  min (Some x) (Some y) = min1 x y
  max (Some x) (Some y) = max1 x y
