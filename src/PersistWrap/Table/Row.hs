module PersistWrap.Table.Row where

import Conkin (Tuple)
import qualified Conkin
import qualified Data.Aeson as JSON
import Data.Singletons (SingI, sing, withSingI)
import Data.Singletons.TypeLits (withKnownSymbol)

import PersistWrap.Conkin.Extra (HEq, singToTuple, (==^))
import qualified PersistWrap.Conkin.Extra.Tuple as Tuple
import PersistWrap.Structure (PrimType, deriveConstraint)
import PersistWrap.Table.Column

data BaseValue fk (bc :: BaseColumn) where
  PV :: PrimType p -> BaseValue fk ('Prim p)
  FKV :: fk otherTableName -> BaseValue fk ('ForeignKey otherTableName)
  JSONV :: JSON.Value -> BaseValue fk 'JSON

instance (HEq fk, SingI bc) => Eq (BaseValue fk bc) where
  (==) = go sing
    where
      go :: forall. SBaseColumn bc -> BaseValue fk bc -> BaseValue fk bc -> Bool
      go (SPrim n) (PV pl) (PV pr) = deriveConstraint @Eq n $ pl == pr
      go (SForeignKey sym) (FKV il) (FKV ir) = withKnownSymbol sym $ il ==^ ir
      go SJSON (JSONV vl) (JSONV vr) = vl == vr

data Value fk (c :: Column) where
  V :: BaseValue fk bc -> Value fk ('Column sym 'False bc)
  N :: Maybe (BaseValue fk bc) -> Value fk ('Column sym 'True bc)

instance (HEq fk, SingI c) => Eq (Value fk c) where
  (==) = case (sing :: SColumn c) of
      SColumn _ _ sctype -> withSingI sctype go
    where
      go :: forall bc sym n. SingI bc
        => Value fk ('Column sym n bc) -> Value fk ('Column sym n bc) -> Bool
      go (N x) (N y) = x == y
      go (V x) (V y) = x == y

newtype MaybeValue fk (c :: Column) = MV (Maybe (Value fk c))

type Row fk (cols :: [Column]) = Tuple cols (Value fk)
type SubRow fk (cols :: [Column]) = Tuple cols (MaybeValue fk)

matches
  :: forall fk xs . (HEq fk, SingI xs) => Tuple xs (MaybeValue fk) -> Tuple xs (Value fk) -> Bool
matches l r = and $ Tuple.zipUncheck (\(MV x) y -> maybe True (== y) x) l r

unrestricted :: SSchema schema -> SubRow fk (SchemaCols schema)
unrestricted (SSchema _ scols) = Conkin.fmap (const $ MV Nothing) (singToTuple scols)
