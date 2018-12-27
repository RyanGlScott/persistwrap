{-# LANGUAGE AllowAmbiguousTypes #-}

module PersistWrap.Table.Row where

import Conkin (Tuple)
import qualified Conkin
import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty(..))
import Data.Singletons (SingI, SingInstance(SingInstance), sing, singInstance)
import Data.Singletons.Prelude (Fst, SList, Sing(STuple2))
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits (Symbol)

import Consin (AlwaysS, compare1, showsPrec1, singToTuple, (==*))
import qualified Consin
import qualified Consin as Tuple (zipUncheckSing)
import PersistWrap.Primitives (PrimType, deriveConstraint)
import PersistWrap.Table.Aeson.Orphans ()
import PersistWrap.Table.Schema
import PersistWrap.Table.EnumVal

data BaseValue fk (bc :: BaseColumn Symbol) where
  PV ::PrimType p -> BaseValue fk ('Prim p)
  EV ::EnumVal (name ': names) -> BaseValue fk ('Enum (name ':| names))
  FKV ::fk otherTableName -> BaseValue fk ('ForeignKey otherTableName)
  JSONV ::JSON.Value -> BaseValue fk 'JSON

instance (SingI bc, AlwaysS Show fk) => Show (BaseValue fk bc) where
  showsPrec d bv = showParen (d > 10) $ case (sing @bc, bv) of
    (SPrim sp, PV p) -> showString "PV " . deriveConstraint @Show sp showsPrec 11 p
    (SEnum ((singInstance -> SingInstance) :%| (singInstance -> SingInstance)), EV ev) ->
      showString "EV " . showsPrec 11 ev
    (SForeignKey (singInstance -> SingInstance), FKV fk) -> showString "FKV " . showsPrec1 11 fk
    (SJSON, JSONV v) -> showString "JSONV " . showsPrec 11 v

instance (AlwaysS Eq fk, SingI bc) => Eq (BaseValue fk bc) where
  (==) = go sing
    where
      go :: SBaseColumn bc -> BaseValue fk bc -> BaseValue fk bc -> Bool
      go (SPrim n) (PV    pl) (PV    pr) = deriveConstraint @Eq n (==) pl pr
      go SEnum{}   (EV    x ) (EV    y ) = x == y
      go (SForeignKey (singInstance -> SingInstance)) (FKV il) (FKV ir) = il ==* ir
      go SJSON     (JSONV vl) (JSONV vr) = vl == vr

instance (AlwaysS Eq fk, AlwaysS Ord fk, SingI bc) => Ord (BaseValue fk bc) where
  compare = go sing
    where
      go :: SBaseColumn bc -> BaseValue fk bc -> BaseValue fk bc -> Ordering
      go (SPrim n) (PV    pl) (PV    pr) = deriveConstraint @Ord n compare pl pr
      go SEnum{}   (EV    x ) (EV    y ) = compare x y
      go (SForeignKey (singInstance -> SingInstance)) (FKV il) (FKV ir) = compare1 il ir
      go SJSON     (JSONV vl) (JSONV vr) = vl `compare` vr


data Value fk (c :: Column Symbol) where
  V ::BaseValue fk bc -> Value fk ('Column 'False bc)
  N ::Maybe (BaseValue fk bc) -> Value fk ('Column 'True bc)

instance (AlwaysS Show fk, SingI c) => Show (Value fk c) where
  showsPrec d v0 = showParen (d > 10) $ case (sing @c, v0) of
    (SColumn _ (singInstance -> SingInstance), V v) -> showString "V " . showsPrec 11 v
    (SColumn _ (singInstance -> SingInstance), N v) -> showString "N " . showsPrec 11 v

instance (AlwaysS Eq fk, SingI c) => Eq (Value fk c) where
  (==) = case sing @c of
    SColumn _ (singInstance -> SingInstance) -> go
    where
      go :: forall bc n . SingI bc => Value fk ( 'Column n bc) -> Value fk ( 'Column n bc) -> Bool
      go (N x) (N y) = x == y
      go (V x) (V y) = x == y

instance (AlwaysS Eq fk, AlwaysS Ord fk, SingI c) => Ord (Value fk c) where
  compare = case sing @c of
    SColumn _ (singInstance -> SingInstance) -> go
    where
      go
        :: forall bc n
         . SingI bc
        => Value fk ( 'Column n bc)
        -> Value fk ( 'Column n bc)
        -> Ordering
      go (N x) (N y) = compare x y
      go (V x) (V y) = compare x y

data ValueSnd fk (nc :: (Symbol,Column Symbol)) where
  ValueSnd ::Value fk col -> ValueSnd fk '(name,col)
instance (AlwaysS Eq fk, SingI nc) => Eq (ValueSnd fk nc) where
  (==) (ValueSnd x) (ValueSnd y) = colEq @(Fst nc) x y
instance (AlwaysS Show fk, SingI nc) => Show (ValueSnd fk nc) where
  showsPrec d (ValueSnd x) = showParen (d > 10) $ showString "ValueSnd " . case sing @nc of
    STuple2 _ (singInstance -> SingInstance) -> showsPrec 11 x
instance AlwaysS Show fk => AlwaysS Show (ValueSnd fk) where
  withAlwaysS (singInstance -> SingInstance) = id

data MaybeValueSnd fk (nc :: (Symbol,Column Symbol)) where
  MaybeValueSnd ::Maybe (Value fk col) -> MaybeValueSnd fk '(name,col)

type Row fk (cols :: [(Symbol, Column Symbol)]) = Tuple cols (ValueSnd fk)
type SubRow fk (cols :: [(Symbol, Column Symbol)]) = Tuple cols (MaybeValueSnd fk)

data ForeignRow fk schema where
  ForeignRow ::fk name -> Row fk cols -> ForeignRow fk ('Schema name cols)

matches
  :: forall fk xs
   . (AlwaysS Eq fk, SingI xs)
  => Tuple xs (MaybeValueSnd fk)
  -> Tuple xs (ValueSnd fk)
  -> Bool
matches l r = and $ Tuple.zipUncheckSing
  (\(MaybeValueSnd x0) y -> case x0 of
    Nothing -> True
    Just x  -> ValueSnd x == y
  )
  l
  r

colEq
  :: forall name col fk
   . (AlwaysS Eq fk, SingI '((name :: Symbol),(col :: Column Symbol)))
  => Value fk col
  -> Value fk col
  -> Bool
colEq = case sing @'(name,col) of
  STuple2 _ ((singInstance -> SingInstance) :: SColumn col) -> (==)

unrestricted :: SList cols -> SubRow fk cols
unrestricted scols = Conkin.fmap (\(STuple2 _ _) -> MaybeValueSnd Nothing) (singToTuple scols)
