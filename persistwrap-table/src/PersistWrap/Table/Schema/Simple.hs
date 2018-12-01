module PersistWrap.Table.Schema.Simple
    ( SimpleColUnnamedType(..)
    , SimpleColType(..)
    , fromSchema
    , toSchema
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text

import PersistWrap.Primitives
import PersistWrap.Table.Schema.Internal hiding (Enum, JSON)
import qualified PersistWrap.Table.Schema.Internal as Schema

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
  | Enum [String]
  | JSON
  | Key String
  | Nullable SimpleColUnnamedType
  deriving (Show)

data SimpleColType = String ::: SimpleColUnnamedType
  deriving (Show)

fromSchema :: Schema Text -> (String, [SimpleColType])
fromSchema (Schema name cols) = (Text.unpack name, map fromCol cols)

fromCol :: (Text, Column Text) -> SimpleColType
fromCol (colName, Column n bc) =
  Text.unpack colName ::: (if n then Nullable else id) (fromBaseCol bc)

fromBaseCol :: BaseColumn Text -> SimpleColUnnamedType
fromBaseCol = \case
  Prim        pn   -> fromPrimitive pn
  Schema.Enum opts -> Enum $ map Text.unpack (NonEmpty.toList opts)
  ForeignKey  fk   -> Key $ Text.unpack fk
  Schema.JSON      -> JSON

fromPrimitive :: PrimName -> SimpleColUnnamedType
fromPrimitive = \case
  PrimText       -> Text
  PrimByteString -> ByteString
  PrimInt64      -> Int64
  PrimDouble     -> Double
  PrimRational   -> Rational
  PrimBool       -> Bool
  PrimDay        -> Day
  PrimTimeOfDay  -> TimeOfDay
  PrimUTCTime    -> UTCTime

toColumnType :: SimpleColType -> (Text, Column Text)
toColumnType (name ::: untype) = (Text.pack name, Column nullability (bt untype))
  where
    nullability = case untype of
      Nullable{} -> True
      _          -> False
    bt = \case
      Text              -> Prim PrimText
      ByteString        -> Prim PrimByteString
      Int64             -> Prim PrimInt64
      Double            -> Prim PrimDouble
      Rational          -> Prim PrimRational
      Bool              -> Prim PrimBool
      Day               -> Prim PrimDay
      TimeOfDay         -> Prim PrimTimeOfDay
      UTCTime           -> Prim PrimUTCTime
      Enum []           -> error "Empty enum options"
      Enum (opt : opts) -> Schema.Enum $ Text.pack opt :| map Text.pack opts
      JSON              -> Schema.JSON
      Key      s        -> ForeignKey $ Text.pack s
      Nullable n        -> bt n

toSchema :: String -> [SimpleColType] -> Schema Text
toSchema name cols = Schema (Text.pack name) (map toColumnType cols)
