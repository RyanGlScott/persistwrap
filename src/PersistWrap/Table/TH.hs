{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Table.TH
    ( SimpleColUnnamedType(..)
    , SimpleColType(..)
    , row
    , schema
    ) where

import Conkin (Tuple (..))
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Singletons (sing)
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Database.Persist.Types (PersistValue (..))
import Language.Haskell.TH (Exp (ListE, VarE), Q, TyLit (StrTyLit), Type (LitT))

import PersistWrap.Structure hiding (List, Map, Prim)
import PersistWrap.Table.Column hiding (JSON)
import qualified PersistWrap.Table.Column as Column
import PersistWrap.Table.Row (BaseValue (..), Value (..))

data SimpleColUnnamedType
  = Text
  | ByteString
  | Int64
  | Double
  | Rational
  | Bool
  | Day
  | TimeOfDay
  | UTCTime
  | Null
  | List
  | Map
  | ObjectId
  | DbSpecific
  | JSON
  | Key String
  | Nullable SimpleColUnnamedType

data SimpleColType = (:::) String SimpleColUnnamedType

toSColumnExpr :: SimpleColType -> Q Exp
toSColumnExpr (name ::: untype) =
    [| sing :: SColumn ('Column $(return (LitT (StrTyLit name))) $(nullability) $(bt untype)) |]
  where
    nullability = case untype of
      Nullable{} -> [t| 'True |]
      _          -> [t| 'False |]
    bt = \case
      Text       -> [t| 'Prim 'PrimText |]
      ByteString -> [t| 'Prim 'PrimByteString |]
      Int64      -> [t| 'Prim 'PrimInt64 |]
      Double     -> [t| 'Prim 'PrimDouble |]
      Rational   -> [t| 'Prim 'PrimRational |]
      Bool       -> [t| 'Prim 'PrimBool |]
      Day        -> [t| 'Prim 'PrimDay |]
      TimeOfDay  -> [t| 'Prim 'PrimTimeOfDay |]
      UTCTime    -> [t| 'Prim 'PrimUTCTime |]
      Null       -> [t| 'Prim 'PrimNull |]
      List       -> [t| 'Prim 'PrimList |]
      Map        -> [t| 'Prim 'PrimMap |]
      ObjectId   -> [t| 'Prim 'PrimObjectId |]
      DbSpecific -> [t| 'Prim 'PrimDbSpecific |]
      JSON       -> [t| 'Column.JSON |]
      Key      s -> [t| 'ForeignKey $(return $ LitT (StrTyLit s)) |]
      Nullable n -> bt n

schema :: String -> [SimpleColType] -> Q Exp
schema s exps = do
  let go = \case
        []     -> [| SNil |]
        x : xs -> do
          x' <- toSColumnExpr x
          [| $(return x') `SCons` $(go xs) |]
  [| SSchemaCon $ SSchema (SSym :: SSymbol $(return $ LitT (StrTyLit s))) $(go exps) |]

class BCValue (bc :: BaseColumn) where
  type DataType (fk :: Symbol -> *) bc :: *
  asBaseValue :: DataType fk bc -> BaseValue fk bc
instance BCValue ('Prim 'PrimText) where
  type DataType fk ('Prim 'PrimText) = Text
  asBaseValue = PV
instance BCValue ('Prim 'PrimByteString) where
  type DataType fk ('Prim 'PrimByteString) = ByteString
  asBaseValue = PV
instance BCValue ('Prim 'PrimInt64) where
  type DataType fk ('Prim 'PrimInt64) = Int64
  asBaseValue = PV
instance BCValue ('Prim 'PrimDouble) where
  type DataType fk ('Prim 'PrimDouble) = Double
  asBaseValue = PV
instance BCValue ('Prim 'PrimRational) where
  type DataType fk ('Prim 'PrimRational) = Rational
  asBaseValue = PV
instance BCValue ('Prim 'PrimBool) where
  type DataType fk ('Prim 'PrimBool) = Bool
  asBaseValue = PV
instance BCValue ('Prim 'PrimDay) where
  type DataType fk ('Prim 'PrimDay) = Day
  asBaseValue = PV
instance BCValue ('Prim 'PrimTimeOfDay) where
  type DataType fk ('Prim 'PrimTimeOfDay) = TimeOfDay
  asBaseValue = PV
instance BCValue ('Prim 'PrimUTCTime) where
  type DataType fk ('Prim 'PrimUTCTime) = UTCTime
  asBaseValue = PV
instance BCValue ('Prim 'PrimList) where
  type DataType fk ('Prim 'PrimList) = [PersistValue]
  asBaseValue = PV
instance BCValue ('Prim 'PrimMap) where
  type DataType fk ('Prim 'PrimMap) = [(Text, PersistValue)]
  asBaseValue = PV
instance BCValue 'Column.JSON where
  type DataType fk 'Column.JSON = JSON.Value
  asBaseValue = JSONV
instance BCValue ('ForeignKey name) where
  type DataType fk ('ForeignKey name) = fk name
  asBaseValue = FKV

asValue
  :: forall name nullability fk bc
   . (SingI nullability, BCValue bc)
  => DataType fk bc
  -> Value fk ( 'Column name nullability bc)
asValue = case (sing :: SBool nullability) of
  SFalse -> V . asBaseValue
  STrue  -> N . Just . asBaseValue

row :: Q Exp -> Q Exp
row = (=<<) $ \case
  ListE exps -> do
    let go = \case
          []     -> [| Nil |]
          x : xs -> do
            x' <- case x of
              VarE n | n == 'null -> [| N Nothing |]
              _                  -> [| asValue $(return x) |]
            [| $(return x') `Cons` $(go xs) |]
    go exps
  e -> error $ "Not a list expression: " ++ show e
--
