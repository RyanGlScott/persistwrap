{-# LANGUAGE AllowAmbiguousTypes #-}

module PersistWrap.Table.Row where

import Conkin (Tuple)
import qualified Conkin
import qualified Data.Aeson as JSON
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.Prelude (Fst, STuple2, Sing(SCons, STuple2))
import Data.Singletons.TypeLits (Symbol)

import PersistWrap.Aeson.Extra ()
import PersistWrap.Conkin.Extra (Always, compare1, showsPrec1, singToTuple, (==*))
import qualified PersistWrap.Conkin.Extra.Tuple as Tuple
import PersistWrap.Structure (PrimType, deriveConstraint)
import PersistWrap.Table.Column
import PersistWrap.Table.EnumVal

data BaseValue fk (bc :: BaseColumn) where
  PV :: PrimType p -> BaseValue fk ('Prim p)
  EV :: EnumVal (name ': names) -> BaseValue fk ('Enum name names)
  FKV :: fk otherTableName -> BaseValue fk ('ForeignKey otherTableName)
  JSONV :: JSON.Value -> BaseValue fk 'JSON

instance (SingI bc, Always Show fk) => Show (BaseValue fk bc) where
  showsPrec d bv = showParen (d > 10) $ case (sing :: SBaseColumn bc, bv) of
    (SPrim sp, PV p) -> showString "PV " . deriveConstraint @Show sp showsPrec 11 p
    (SEnum n1 nr, EV ev) -> showString "EV " . withSingI (n1 `SCons` nr) showsPrec 11 ev
    (SForeignKey sfk, FKV fk) -> showString "FKV " . withSingI sfk showsPrec1 11 fk
    (SJSON, JSONV v) -> showString "JSONV " . showsPrec 11 v

instance (Always Eq fk, SingI bc) => Eq (BaseValue fk bc) where
  (==) = go sing
    where
      go :: forall. SBaseColumn bc -> BaseValue fk bc -> BaseValue fk bc -> Bool
      go (SPrim n) (PV pl) (PV pr) = deriveConstraint @Eq n (==) pl pr
      go (SEnum opt opts) (EV x) (EV y) = withSingI (opt `SCons` opts) (==) x y
      go (SForeignKey sym) (FKV il) (FKV ir) = withSingI sym (==*) il ir
      go SJSON (JSONV vl) (JSONV vr) = vl == vr

instance (Always Eq fk, Always Ord fk, SingI bc) => Ord (BaseValue fk bc) where
  compare = go sing
    where
      go :: forall. SBaseColumn bc -> BaseValue fk bc -> BaseValue fk bc -> Ordering
      go (SPrim n) (PV pl) (PV pr) = deriveConstraint @Ord n compare pl pr
      go (SEnum opt opts) (EV x) (EV y) = withSingI (opt `SCons` opts) compare x y
      go (SForeignKey sym) (FKV il) (FKV ir) = withSingI sym compare1 il ir
      go SJSON (JSONV vl) (JSONV vr) = vl `compare` vr


data Value fk (c :: Column) where
  V :: BaseValue fk bc -> Value fk ('Column 'False bc)
  N :: Maybe (BaseValue fk bc) -> Value fk ('Column 'True bc)

instance (Always Show fk, SingI c) => Show (Value fk c) where
  showsPrec d v0 = showParen (d > 10) $ case (sing :: SColumn c, v0) of
    (SColumn _ sbc, V v) -> showString "V " . withSingI sbc showsPrec 11 v
    (SColumn _ sbc, N v) -> showString "N " . withSingI sbc showsPrec 11 v

instance (Always Eq fk, SingI c) => Eq (Value fk c) where
  (==) = case (sing :: SColumn c) of
      SColumn _ sctype -> withSingI sctype go
    where
      go :: forall bc n. SingI bc
        => Value fk ('Column n bc) -> Value fk ('Column n bc) -> Bool
      go (N x) (N y) = x == y
      go (V x) (V y) = x == y

instance (Always Eq fk, Always Ord fk, SingI c) => Ord (Value fk c) where
  compare = case (sing :: SColumn c) of
      SColumn _ sctype -> withSingI sctype go
    where
      go :: forall bc n. SingI bc
        => Value fk ('Column n bc) -> Value fk ('Column n bc) -> Ordering
      go (N x) (N y) = compare x y
      go (V x) (V y) = compare x y

data ValueSnd fk (nc :: (Symbol,Column)) where
  ValueSnd :: Value fk col -> ValueSnd fk '(name,col)
instance (Always Eq fk, SingI nc) => Eq (ValueSnd fk nc) where
  (==) (ValueSnd x) (ValueSnd y) = colEq @(Fst nc) x y
instance (Always Show fk, SingI nc) => Show (ValueSnd fk nc) where
  showsPrec d (ValueSnd x) = showParen (d > 10) $ showString "ValueSnd " . case sing :: Sing nc of
    STuple2 _ scol -> withSingI scol (showsPrec 11 x)

data MaybeValueSnd fk (nc :: (Symbol,Column)) where
  MaybeValueSnd :: Maybe (Value fk col) -> MaybeValueSnd fk '(name,col)

type Row fk (cols :: [(Symbol,Column)]) = Tuple cols (ValueSnd fk)
type SubRow fk (cols :: [(Symbol,Column)]) = Tuple cols (MaybeValueSnd fk)

matches
  :: forall fk xs
   . (Always Eq fk, SingI xs)
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
   . (Always Eq fk, SingI '((name :: Symbol),(col :: Column)))
  => Value fk col
  -> Value fk col
  -> Bool
colEq = case sing :: STuple2 '(name,col) of
  STuple2 _ (sc :: SColumn col) -> withSingI sc (==)

unrestricted :: SSchema schema -> SubRow fk (SchemaCols schema)
unrestricted (SSchema _ scols) =
  Conkin.fmap (\(STuple2 _ _) -> MaybeValueSnd Nothing) (singToTuple scols)
