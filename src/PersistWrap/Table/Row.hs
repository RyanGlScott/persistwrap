module PersistWrap.Table.Row where

import Conkin (Tuple)

import qualified PersistWrap.Conkin.Extra as Tuple
import PersistWrap.Structure (PrimType)
import PersistWrap.Table.Column

data Value (c :: Column) where
    V :: Eq (PrimType p) => PrimType p -> Value ('Column sym 'False p)
    N :: Eq (PrimType p) => Maybe (PrimType p) -> Value ('Column sym 'True p)

deriving instance Eq (Value c)

newtype MaybeValue c = MV (Maybe (Value c))

type Row (sch :: Schema) = Tuple (Cols sch) Value
type SubRow (sch :: Schema) = Tuple (Cols sch) MaybeValue

matches :: forall xs. Tuple xs MaybeValue -> Tuple xs Value -> Bool
matches l r = and $ Tuple.zipUncheck (\(MV x) y -> case x of
    Nothing -> True
    Just x' -> x' == y
  ) l r
