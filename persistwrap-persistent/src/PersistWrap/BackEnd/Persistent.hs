{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.BackEnd.Persistent
    ( SqlQuery
    ) where

import Conkin (Tagged(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON
import Database.Persist.TH (derivePersistFieldJSON)
import Data.Maybe (mapMaybe, isJust)
import Database.Persist (BackendKey, PersistEntity, PersistField, PersistValue(..), (==.))
import qualified Database.Persist as BackEnd
import Database.Persist.Sql (SqlBackend, SqlType(..))
import qualified Database.Persist.Sql as BackEnd
import Data.Pool (Pool)
import Data.Proxy (Proxy)
import Data.Singletons
import Data.Singletons.Prelude (Sing(SCons, SFalse, STrue, STuple2), Snd)
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits (Symbol)
import Data.Text (Text)
import qualified Data.Text as Text

import Conkin.Extra (Index, htraverse, tagCases)
import qualified Conkin.Extra as Tuple (findJust)
import Consin (AlwaysS(..), IndexedItemS(..), getWithIndexS, getTaggedValueS, singToTuple)
import Consin.SingMap (SingMap)
import qualified Consin.SingMap as SingMap
import PersistWrap.Primitives
import PersistWrap.Table
import PersistWrap.Table.Class (MonadBaseTransaction)
import qualified PersistWrap.Table.Class

type TableMap = SingMap (SomeTableNamed (Table SqlQuery))

newtype SqlQuery x = SqlQuery (ReaderT SqlBackend (ReaderT TableMap IO) x)
  deriving (Functor, Applicative, Monad)

newtype PersistentFK (name :: Symbol) = PersistentFK (BackendKey SqlBackend)
  deriving (Eq, Ord)
instance AlwaysS Eq PersistentFK where withAlwaysS = const id
instance AlwaysS Ord PersistentFK where withAlwaysS = const id

type instance ForeignKey SqlQuery = PersistentFK

newtype NamedRow schema = NamedRow (Row PersistentFK (SchemaCols schema))

newtype JSONValue = JSONValue {unJSONValue :: JSON.Value}
  deriving (FromJSON, ToJSON)
$(derivePersistFieldJSON "JSONValue")

instance SingI bc => PersistField (BaseValue PersistentFK bc) where
  toPersistValue = case sing @_ @bc of
    SPrim pn -> \(PV v) -> case pn of
      SPrimText       -> PersistText v
      SPrimByteString -> PersistByteString v
      SPrimInt64      -> PersistInt64 v
      SPrimDouble     -> PersistDouble v
      SPrimRational   -> PersistRational v
      SPrimBool       -> PersistBool v
      SPrimDay        -> PersistDay v
      SPrimTimeOfDay  -> PersistTimeOfDay v
      SPrimUTCTime    -> PersistUTCTime v
    SEnum ((singInstance -> SingInstance) :%| (singInstance -> SingInstance)) ->
      \(EV (EnumVal v)) ->
        PersistText $ getTaggedValueS (\(_ :: Proxy name) -> fromSing (sing @_ @name)) v
    SForeignKey _ -> \(FKV (PersistentFK k)) -> BackEnd.toPersistValue k
    SJSON         -> \(JSONV v) -> BackEnd.toPersistValue (JSONValue v)
  fromPersistValue =
    let
      s = sing @_ @bc
      failureMessage :: PersistValue -> Text
      failureMessage v =
        Text.pack $ "Unexpected value " ++ show v ++ "for type " ++ show (fromSing s)
    in
      case s of
        SPrim pn -> case pn of
          SPrimText -> \case
            PersistText v -> Right $ PV v
            v             -> Left $ failureMessage v
          SPrimByteString -> \case
            PersistByteString v -> Right $ PV v
            v                   -> Left $ failureMessage v
          SPrimInt64 -> \case
            PersistInt64 v -> Right $ PV v
            v              -> Left $ failureMessage v
          SPrimDouble -> \case
            PersistDouble v -> Right $ PV v
            v               -> Left $ failureMessage v
          SPrimRational -> \case
            PersistRational v -> Right $ PV v
            v                 -> Left $ failureMessage v
          SPrimBool -> \case
            PersistBool v -> Right $ PV v
            v             -> Left $ failureMessage v
          SPrimDay -> \case
            PersistDay v -> Right $ PV v
            v            -> Left $ failureMessage v
          SPrimTimeOfDay -> \case
            PersistTimeOfDay v -> Right $ PV v
            v                  -> Left $ failureMessage v
          SPrimUTCTime -> \case
            PersistUTCTime v -> Right $ PV v
            v                -> Left $ failureMessage v
        SEnum (name :%| names) ->
          let listNames = name `SCons` names
          in
            \case
              PersistText t ->
                case
                    Tuple.findJust (\sx -> if fromSing sx == t then Just Proxy else Nothing)
                                   (singToTuple listNames)
                  of
                    Nothing ->
                      Left $ Text.unwords
                        [t, "is not a member of", Text.pack $ show (fromSing listNames)]
                    Just x -> Right $ EV $ EnumVal x
              v -> Left $ failureMessage v
        SForeignKey{} -> fmap (FKV . PersistentFK) . BackEnd.fromPersistValue
        SJSON{}       -> fmap (JSONV . unJSONValue) . BackEnd.fromPersistValue

instance SingI col => PersistField (Value PersistentFK col) where
  toPersistValue = case sing @_ @col of
    SColumn SFalse (singInstance -> SingInstance) -> \case
      V bv -> BackEnd.toPersistValue bv
    SColumn STrue (singInstance -> SingInstance) -> \case
      N mbv -> case mbv of
        Nothing -> PersistNull
        Just bv -> BackEnd.toPersistValue bv
  fromPersistValue = case sing @_ @col of
    SColumn SFalse (singInstance -> SingInstance) -> fmap V . BackEnd.fromPersistValue
    SColumn STrue (singInstance -> SingInstance) -> fmap N . \case
      PersistNull -> pure Nothing
      x -> BackEnd.fromPersistValue x

instance SingI schema => PersistEntity (NamedRow schema) where
  type PersistEntityBackend (NamedRow schema) = SqlBackend
  newtype Key (NamedRow schema) = NamedRowKey (BackendKey SqlBackend)
    deriving (Eq, Ord, Read, Show, PersistField, ToJSON, FromJSON)
  data EntityField (NamedRow schema) typ where
    PrimKeyField :: BackEnd.EntityField (NamedRow schema) (BackEnd.Key (NamedRow schema))
    ColField
      :: Index cols col
      -> BackEnd.EntityField (NamedRow ('Schema name cols)) (Value PersistentFK (Snd col))
  data Unique (NamedRow schema)
  keyToValues (NamedRowKey k) = [BackEnd.toPersistValue k]
  keyFromValues = \case
    [k] -> NamedRowKey <$> BackEnd.fromPersistValue k
    _ -> Left "Unexpected list length"
  persistIdField = PrimKeyField
  entityDef _ =
    let haskellName = BackEnd.HaskellName $ Text.unwords
          ["NamedRow", Text.pack $ showParen True (showPromotedSchema (sing @_ @schema)) ""]
    in
      BackEnd.EntityDef
        { entityHaskell  = haskellName
        , entityDB       = BackEnd.DBName $ case sing @_ @schema of
                             SSchema name _ -> fromSing name
        , entityId       =
          BackEnd.FieldDef
            { fieldHaskell   = BackEnd.HaskellName "Id"
            , fieldDB        = BackEnd.DBName "id"
            , fieldType      = BackEnd.FTTypeCon Nothing "NamedRowId"
            , fieldSqlType   = BackEnd.SqlInt64
            , fieldAttrs     = []
            , fieldStrict    = False
            , fieldReference = BackEnd.ForeignRef haskellName
                                                  (BackEnd.FTTypeCon (Just "Data.Int") "Int64")
            }
        , entityAttrs    = []
        , entityFields   = case fromSing (sing @_ @schema) of
                             Schema _ cols -> flip map cols $ \(name, Column nullability bc) ->
                               BackEnd.FieldDef
                                 { fieldHaskell   = BackEnd.HaskellName name
                                 , fieldDB        = BackEnd.DBName name
                                 , fieldType = BackEnd.FTTypeCon Nothing ("NamedRowField_" <> name)
                                 , fieldSqlType   = case bc of
                                                      Prim pn -> case pn of
                                                        PrimText       -> SqlString
                                                        PrimByteString -> SqlBlob
                                                        PrimInt64      -> SqlInt64
                                                        PrimDouble     -> SqlReal
                                                        PrimRational   -> _
                                                        PrimBool       -> SqlBool
                                                        PrimDay        -> SqlDay
                                                        PrimTimeOfDay  -> SqlTime
                                                        PrimUTCTime    -> SqlDayTime
                                                      Enum       _ -> SqlString
                                                      ForeignKey _ -> SqlInt64
                                                      JSON         -> SqlBlob
                                 , fieldAttrs     = [ _ | nullability ]
                                 , fieldStrict    = False
                                 , fieldReference = case bc of
                                                      ForeignKey _ -> _
                                                      _            -> BackEnd.NoReference
                                 }
        , entityUniques  = []
        , entityForeigns = _
        , entityDerives  = []
        , entityExtra    = _
        , entitySum      = False
        }

instance MonadBaseTransaction SqlQuery where
  data Key SqlQuery tab = KeyPersistent (BackEnd.Key (NamedRow (TabSchema tab)))
  data Table SqlQuery schema = TablePersistent
  getEntities proxy r = do
    results <- SqlQuery (BackEnd.selectList (makeFilters proxy r) [])
    return $ map (\(BackEnd.Entity k (NamedRow v)) -> Entity (KeyPersistent k) v) results
  getRow (KeyPersistent k) = fmap (\(NamedRow r) -> r) <$> SqlQuery (BackEnd.get k)
  insertRow _ row = KeyPersistent <$> SqlQuery (BackEnd.insert (NamedRow row))
  deleteRow k0@(KeyPersistent k) = do
    wasPresent <- isJust <$> getRow k0
    SqlQuery (BackEnd.delete k)
    pure wasPresent
  stateRow k0@(KeyPersistent k) op =
    getRow k0 >>= \case
      Nothing -> pure Nothing
      Just v -> do
        let (y, v') = op v
        SqlQuery (BackEnd.replace k (NamedRow v'))
        pure $ Just y
  lookupTable name = SqlQuery $ lift $ asks $ SingMap.lookup name
  keyToForeign (KeyPersistent (NamedRowKey k)) = PersistentFK k
  foreignToKey _ (PersistentFK k) = KeyPersistent $ NamedRowKey k

instance MonadTransaction SqlQuery where
  type BaseTransactionMonad SqlQuery = SqlQuery
  liftTransaction = id

makeFilters
  :: forall tab
   . SingI (TabSchema tab)
  => Proxy tab
  -> SubRow PersistentFK (TabCols tab)
  -> [BackEnd.Filter (NamedRow (TabSchema tab))]
makeFilters _ = case sing @_ @(TabSchema tab) of
  SSchema _ (singInstance -> SingInstance) ->
    mapMaybe (fmap taggedToFilter . htraverse traverseMaybeValueSnd) . tagCases

traverseMaybeValueSnd :: MaybeValueSnd PersistentFK nc -> Maybe (ValueSnd PersistentFK nc)
traverseMaybeValueSnd (MaybeValueSnd x) = ValueSnd <$> x

taggedToFilter
  :: SingI cols
  => Tagged cols (ValueSnd PersistentFK)
  -> BackEnd.Filter (NamedRow ( 'Schema name cols))
taggedToFilter (getWithIndexS -> IndexedItemS (i :: Index cols nc) (ValueSnd x)) =
  case sing @_ @nc of
    STuple2 _ (singInstance -> SingInstance) -> ColField i ==. x

newtype PersistentT m a = PersistentT (ReaderT (Pool SqlBackend) (ReaderT TableMap m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runPersistentT :: Pool SqlBackend -> TableMap -> PersistentT m a -> m a
runPersistentT pool tableMap (PersistentT act) = runReaderT (runReaderT act pool) tableMap

type instance ForeignKey (PersistentT m) = PersistentFK

instance MonadIO m => MonadPersist (PersistentT m) where
  type Transaction (PersistentT m) = SqlQuery
  atomicTransaction (SqlQuery act) = PersistentT $ do
    pool <- ask
    tm <- lift ask
    liftIO $ runReaderT (BackEnd.runSqlPool act pool) tm
