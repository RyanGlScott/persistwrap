{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.BackEnd.Persistent.NamedRow
    ( EntityField(ColField, PrimKeyField)
    , BackEnd.Key(NamedRowKey)
    , NamedRow(..)
    , PersistentFK(..)
    , namedRowEntity
    ) where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON
import Database.Persist.TH (derivePersistFieldJSON)
import Database.Persist as BackEnd
import Database.Persist.Sql (PersistFieldSql, SqlBackend, SqlType, sqlType)
import qualified Data.Map as Map
import Data.Singletons
import Data.Singletons.Prelude (Sing(SCons, SFalse, STrue, STuple2), Snd)
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits (Symbol)
import Data.Text (Text)
import qualified Data.Text as Text

import Conkin.Extra (Index, tupleLens, uncheckIndex, zipWithUncheckedM)
import qualified Conkin.Extra as Tuple (findJust)
import Consin (AlwaysS(..), getTaggedValueS, mapUncheckSing, singToTuple, withAlwaysSTupleShow)
import PersistWrap.Primitives
import PersistWrap.Table

newtype PersistentFK s (name :: Symbol) = PersistentFK (BackendKey SqlBackend)
  deriving (Eq, Ord, Show)
instance AlwaysS Eq (PersistentFK s) where
  withAlwaysS = const id
instance AlwaysS Ord (PersistentFK s) where
  withAlwaysS = const id
instance AlwaysS Show (PersistentFK s) where
  withAlwaysS = const id

newtype JSONValue = JSONValue {unJSONValue :: JSON.Value}
  deriving (FromJSON, ToJSON)
$(derivePersistFieldJSON "JSONValue")

newtype NamedRow s schema = NamedRow (Row (PersistentFK s) (SchemaCols schema))

deriving instance Show (Row (PersistentFK s) (SchemaCols schema)) => Show (NamedRow s schema)

instance AlwaysS Show (NamedRow s) where
  withAlwaysS (SSchema _ (singInstance -> SingInstance :: SingInstance cols)) =
    withAlwaysSTupleShow @cols @(ValueSnd (PersistentFK s))

typeName :: SqlType -> Text
typeName = undefined

fieldDef :: (Text, Column Text) -> FieldDef
fieldDef (name, c@(Column nullability bc)) =
  let fSqlType = colSqlType c
  in  FieldDef
        { fieldHaskell   = HaskellName name
        , fieldDB        = DBName name
        , fieldType      = FTTypeCon Nothing (typeName fSqlType)
        , fieldSqlType   = fSqlType
        , fieldAttrs     = [ "Maybe" | nullability ]
        , fieldStrict    = False
        , fieldReference = case bc of
                             ForeignKey fname ->
                               ForeignRef (nrName fname) (FTTypeCon (Just "Data.Int") "Int64")
                             _ -> NoReference
        }

-- Have to use a partial type signature since the column types may not be known.
nrName :: Text -> HaskellName
nrName schName = HaskellName $ "NamedRow s ('Schema " <> Text.pack (show schName) <> " _)"

namedRowEntity :: Schema Text -> EntityDef
namedRowEntity (Schema schname cols) =
  let entityHaskell = nrName schname
      entityId      = FieldDef
        { fieldHaskell   = HaskellName "Id"
        , fieldDB        = DBName "id"
        , fieldType      = FTTypeCon Nothing "NamedRowId"
        , fieldSqlType   = SqlInt64
        , fieldAttrs     = []
        , fieldStrict    = False
        , fieldReference = ForeignRef entityHaskell (FTTypeCon (Just "Data.Int") "Int64")
        }
  in  EntityDef { entityHaskell
                , entityDB       = DBName schname
                , entityId
                , entityAttrs    = []
                , entityFields   = map fieldDef cols
                , entityUniques  = []
                , entityForeigns = []
                , entityDerives  = []
                , entityExtra    = Map.empty
                , entitySum      = False
                }

instance SingI schema => PersistEntity (NamedRow s schema) where
  type PersistEntityBackend (NamedRow s schema) = SqlBackend
  newtype Key (NamedRow s schema) = NamedRowKey (BackendKey SqlBackend)
    deriving (Eq, Ord, Read, Show, PersistField, ToJSON, FromJSON)
  data EntityField (NamedRow s schema) typ where
    PrimKeyField :: EntityField (NamedRow s schema) (BackEnd.Key (NamedRow s schema))
    ColField
      :: Index cols col
      -> EntityField (NamedRow s ('Schema name cols)) (Value (PersistentFK s) (Snd col))
  data Unique (NamedRow s schema)
  keyToValues (NamedRowKey k) = [toPersistValue k]
  keyFromValues = \case
    [k] -> NamedRowKey <$> fromPersistValue k
    _   -> Left "Unexpected list length"
  persistIdField = PrimKeyField
  entityDef _ = namedRowEntity $ fromSing $ sing @_ @schema
  toPersistFields = case sing @_ @schema of
    SSchema _ scols -> \(NamedRow r) -> mapUncheckSing scols SomePersistField r
  fromPersistValues = case sing @_ @schema of
    SSchema _ scols ->
      zipWithUncheckedM (\(singInstance -> SingInstance) -> fromPersistValue) (singToTuple scols)
        >=> maybe (Left "Wrong number of items") (Right . NamedRow)
  persistUniqueToFieldNames = \case {}
  persistUniqueToValues = \case {}
  persistUniqueKeys = const []
  persistFieldDef =
    let def = entityDef (Proxy @(NamedRow s schema))
    in  \case
          PrimKeyField -> entityId def
          ColField i   -> entityFields def !! uncheckIndex i
  fieldLens = \case
    PrimKeyField -> \keyFn (BackEnd.Entity k v) -> (`BackEnd.Entity` v) <$> keyFn k
    ColField i   -> \vFn (BackEnd.Entity k (NamedRow r)) ->
      (k `BackEnd.Entity`) . NamedRow <$> tupleLens i (\(ValueSnd v) -> ValueSnd <$> vFn v) r

instance SingI bc => PersistField (BaseValue (PersistentFK s) bc) where
  toPersistValue = case sing @_ @bc of
    SPrim pn -> deriveConstraint @PersistField pn $ \(PV v) -> toPersistValue v
    SEnum ((singInstance -> SingInstance) :%| (singInstance -> SingInstance)) ->
      \(EV (EnumVal v)) ->
        toPersistValue $ getTaggedValueS (\(_ :: Proxy name) -> fromSing (sing @_ @name)) v
    SForeignKey _ -> \(FKV (PersistentFK k)) -> toPersistValue k
    SJSON         -> \(JSONV v) -> toPersistValue (JSONValue v)
  fromPersistValue = case sing @_ @bc of
    SPrim pn               -> deriveConstraint @PersistField pn $ fmap PV . fromPersistValue
    SEnum (name :%| names) -> \x -> do
      let listNames = name `SCons` names
      t <- fromPersistValue x
      case
          Tuple.findJust (\sx -> if fromSing sx == t then Just Proxy else Nothing)
                         (singToTuple listNames)
        of
          Nothing ->
            Left $ Text.unwords [t, "is not a member of", Text.pack $ show (fromSing listNames)]
          Just v -> Right $ EV $ EnumVal v
    SForeignKey{} -> fmap (FKV . PersistentFK) . fromPersistValue
    SJSON{}       -> fmap (JSONV . unJSONValue) . fromPersistValue

instance SingI col => PersistField (Value (PersistentFK s) col) where
  toPersistValue = case sing @_ @col of
    SColumn SFalse (singInstance -> SingInstance) -> \case
      V bv -> toPersistValue bv
    SColumn STrue (singInstance -> SingInstance) -> \case
      N mbv -> toPersistValue mbv
  fromPersistValue = case sing @_ @col of
    SColumn SFalse (singInstance -> SingInstance) -> fmap V . fromPersistValue
    SColumn STrue  (singInstance -> SingInstance) -> fmap N . fromPersistValue

instance SingI nc => PersistField (ValueSnd (PersistentFK s) nc) where
  toPersistValue = case sing @_ @nc of
    STuple2 _ (singInstance -> SingInstance) -> \(ValueSnd v) -> toPersistValue v
  fromPersistValue = case sing @_ @nc of
    STuple2 _ (singInstance -> SingInstance) -> fmap ValueSnd . fromPersistValue

baseColSqlType :: BaseColumn Text -> SqlType
baseColSqlType = \case
  Prim (toSing -> SomeSing (pn :: SPrimName pn)) ->
    deriveConstraint @PersistFieldSql pn sqlType (Proxy @(PrimType pn))
  Enum       _ -> sqlType (Proxy @Text)
  ForeignKey _ -> sqlType (Proxy @(BackendKey SqlBackend))
  JSON         -> sqlType (Proxy @JSONValue)

colSqlType :: Column Text -> SqlType
colSqlType (Column _ bc) = baseColSqlType bc

instance SingI bc => PersistFieldSql (BaseValue (PersistentFK s) bc) where
  sqlType = const $ baseColSqlType $ fromSing (sing @_ @bc)

instance SingI c => PersistFieldSql (Value (PersistentFK s) c) where
  sqlType = const $ colSqlType $ fromSing (sing @_ @c)

instance SingI nc => PersistFieldSql (ValueSnd (PersistentFK s) nc) where
  sqlType = const $ case fromSing (sing @_ @nc) of
    (_, c) -> colSqlType c
