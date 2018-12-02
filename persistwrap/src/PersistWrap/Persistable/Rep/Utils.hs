module PersistWrap.Persistable.Rep.Utils
    ( ColumnRep(..)
    , NamedColumnRep(..)
    , NamedSchemaRep(..)
    , SchemaRep(..)
    , SingleCol(..)
    , addName
    , bimapCol
    , biProduct
    , biSum
    , biV
    , buildEnumCol
    , buildOptionCol
    , buildSingleProductCol
    , extractUnit
    , hasUnitOption
    , isUnit
    , namedUnderlyingNullable
    ) where

import Prelude hiding ((.))

import Conkin (Tagged(..), Tuple(..))
import qualified Conkin
import Control.Category ((.))
import Data.Bijection
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(Proxy))
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List.NonEmpty (Sing((:%|)))
import Data.Singletons.TypeLits
import GHC.Stack (HasCallStack)

import Conkin.Extra
import PersistWrap.Maybe.Extra (fromJust)
import PersistWrap.Structure as Structure
import PersistWrap.Table as Table

data ColumnRep fk (x :: *) where
  UnitRep :: x -> ColumnRep fk x
  PrimRep :: SColumn c -> ColumnRep fk (Value fk c)
  FnRep :: ColumnRep fk x1 -> x1 :<->: x2 -> ColumnRep fk x2
  ForeignRep :: NamedSchemaRep fk schemaName x -> ColumnRep fk (EntityOf fk x)
  NullForeignRep :: NamedSchemaRep fk schemaName x -> ColumnRep fk (Maybe (EntityOf fk x))
  ListRep
    :: NamedSchemaRep fk schemaName x -> ColumnRep fk (EntityOf fk ('ListType x))
  MapRep
    :: SingI k
    => ColumnRep fk (EntityOf fk k)
    -> NamedSchemaRep fk schemaName v
    -> ColumnRep fk (EntityOf fk ('MapType k v))

data SchemaRep fk structure where
  AtMostOneColumnSchema :: ColumnRep fk (EntityOf fk structure) -> SchemaRep fk structure
  ProductSchema :: Tuple nxs (NamedColumnRep fk) -> SchemaRep fk ('ProductType nxs)
  SumUnIndexedSchema
    :: Maybe (Tagged (nx ': nxs) Proxy) -- Column to use when all columns are null
    -> Tuple (nx ': nxs) (NamedColumnRep fk)
    -> SchemaRep fk ('SumType (nx ':| nxs))
  SumIndexedSchema
    :: Tuple (nx ': nxs) (NamedColumnRep fk) -> SchemaRep fk ('SumType (nx ':| nxs))

data NamedColumnRep fk nx where
  NamedColumnRep
    :: SSymbol n -> ColumnRep fk (EntityOf fk structure) -> NamedColumnRep fk '(n,structure)

data NamedSchemaRep fk schemaName x = NamedSchemaRep (SSymbol schemaName) (SchemaRep fk x)

bimapCol :: x :<->: y -> ColumnRep fk x -> ColumnRep fk y
bimapCol f = \case
  UnitRep x -> UnitRep $ biTo f x
  FnRep c g -> FnRep c (f . g)
  c         -> FnRep c f

biV :: BaseValue fk bc :<->: Value fk ( 'Column 'False bc)
biV = Bi V (\(V x) -> x)

makeNullable :: HasCallStack => ColumnRep fk x -> ColumnRep fk (Maybe x)
makeNullable = \case
  UnitRep{} -> error "No associated column"
  PrimRep (SColumn SFalse bc) ->
    FnRep (PrimRep (SColumn STrue bc)) (bimap biV . Bi (\(N x) -> x) N)
  PrimRep (SColumn STrue _) -> error "Column already nullable"
  FnRep cr fn               -> bimapCol (bimap fn) (makeNullable cr)
  ForeignRep schr           -> NullForeignRep schr
  NullForeignRep{}          -> error "Column already nullable"
  ListRep{}                 -> error "No associated column"
  MapRep{}                  -> error "No associated column"

namedUnderlyingNullable :: NamedColumnRep fk x -> NamedColumnRep fk x
namedUnderlyingNullable (NamedColumnRep name cr) = NamedColumnRep name (underlyingNullable cr)

underlyingNullable :: HasCallStack => ColumnRep fk x -> ColumnRep fk x
underlyingNullable cr0 = case cr0 of
  UnitRep{} -> cr0
  PrimRep (SColumn SFalse bc) ->
    FnRep (PrimRep (SColumn STrue bc)) (Bi fromJust Just . bimap biV . Bi (\(N x) -> x) N)
  PrimRep (SColumn STrue _) -> cr0
  FnRep cr fn               -> FnRep (underlyingNullable cr) fn
  ForeignRep schr           -> FnRep (NullForeignRep schr) (Bi fromJust Just)
  NullForeignRep{}          -> cr0
  ListRep{}                 -> cr0
  MapRep{}                  -> cr0

biProduct :: Tuple xs (EntityOfSnd fk) :<->: EntityOf fk ( 'ProductType xs)
biProduct = Bi Product (\(Product x) -> x)

isUnit :: NamedColumnRep fk x -> Bool
isUnit = \case
  NamedColumnRep _ UnitRep{} -> True
  _                          -> False

hasUnitOption :: forall fk x . NamedColumnRep fk x -> Bool
hasUnitOption (NamedColumnRep _ r) = go r
  where
    go :: forall x' . ColumnRep fk x' -> Bool
    go = \case
      UnitRep{}             -> True
      PrimRep (SColumn n _) -> case n of
        SFalse -> False
        STrue  -> True
      FnRep cr _       -> go cr
      ForeignRep{}     -> False
      NullForeignRep{} -> True
      ListRep{}        -> True
      MapRep{}         -> True

extractUnit :: NamedColumnRep fk x -> Maybe (EntityOfSnd fk x)
extractUnit = \case
  NamedColumnRep _ (UnitRep u) -> Just $ EntityOfSnd u
  _                            -> Nothing

addName :: SSymbol prefix -> SSymbol name -> SSymbol (prefix <> "_" <> name)
addName prefix name = prefix %<> SSym @"_" %<> name

data SingleCol fk xs where
  SingleCol :: ColumnRep fk x -> (x :<->: Tuple xs (EntityOfSnd fk)) -> SingleCol fk xs

buildSingleProductCol :: HasCallStack => Tuple xs (NamedColumnRep fk) -> SingleCol fk xs
buildSingleProductCol = \case
  Nil -> error "No column"
  NamedColumnRep _ (UnitRep x) `Cons` xs -> case buildSingleProductCol xs of
    SingleCol cr fn -> SingleCol cr (Bi (EntityOfSnd x `Cons`) (\(_ `Cons` rest) -> rest) . fn)
  NamedColumnRep _ x `Cons` xs ->
    let rest = Conkin.fmap (fromJust . extractUnit) xs
    in  SingleCol x (Bi (\v -> EntityOfSnd v `Cons` rest) (\(EntityOfSnd v `Cons` _) -> v))

biSum :: Tagged (x ': xs) (EntityOfSnd fk) :<->: EntityOf fk ( 'SumType (x ':| xs))
biSum = Bi Sum (\(Sum x) -> x)

data EnumResult fk xs where
  EnumResult
    :: SList (ns :: [Symbol])
    -> (Tagged ns Proxy :<->: Tagged xs (EntityOfSnd fk))
    -> EnumResult fk xs

buildEnumResult :: HasCallStack => Tuple nxs (NamedColumnRep fk) -> EnumResult fk nxs
buildEnumResult = \case
  Nil                               -> EnumResult SNil (Bi noHere noHere)
  NamedColumnRep name x `Cons` rest -> case x of
    UnitRep v -> case buildEnumResult rest of
      EnumResult restNames restFn -> EnumResult
        (name `SCons` restNames)
        (Bi
          (\case
            Here  Proxy -> Here (EntityOfSnd v)
            There other -> There $ biTo restFn other
          )
          (\case
            Here  _     -> Here Proxy
            There other -> There $ biFrom restFn other
          )
        )
    _ -> error "Not a unit type"

buildEnumCol
  :: HasCallStack
  => Tuple (x ': xs) (NamedColumnRep fk)
  -> ColumnRep fk (EntityOf fk ( 'SumType (x ':| xs)))
buildEnumCol xs = case buildEnumResult xs of
  EnumResult names conversion -> case names of
    SNil                      -> error "no names?"
    nameHead `SCons` nameTail -> FnRep
      (PrimRep (SColumn SFalse (SEnum (nameHead :%| nameTail))))
      (biSum . conversion . Bi (\(EV (EnumVal x)) -> x) (EV . EnumVal) . inverse biV)

makeOption
  :: HasCallStack
  => EntityOfSnd fk nx
  -> NamedColumnRep fk ny
  -> ColumnRep fk (EntityOf fk ( 'SumType (nx ':| '[ny])))
makeOption def (NamedColumnRep _ cr) =
  bimapCol
      (biSum . Bi
        (maybe (Here def) (There . Here . EntityOfSnd))
        (\case
          Here  _                            -> Nothing
          There (getSingle -> EntityOfSnd x) -> Just x
        )
      )
    $ makeNullable cr

swapCases
  :: ColumnRep fk (EntityOf fk ( 'SumType (nx ':| '[ny])))
  -> ColumnRep fk (EntityOf fk ( 'SumType (ny ':| '[nx])))
swapCases = bimapCol (biSum . Bi swapOptions swapOptions . inverse biSum)

buildOptionCol
  :: HasCallStack
  => Tuple (x ': xs) (NamedColumnRep fk)
  -> ColumnRep fk (EntityOf fk ( 'SumType (x ':| xs)))
buildOptionCol = \case
  (NamedColumnRep _ (UnitRep x) `Cons` y `Cons` Nil) -> makeOption (EntityOfSnd x) y
  (x `Cons` NamedColumnRep _ (UnitRep y) `Cons` Nil) -> swapCases $ makeOption (EntityOfSnd y) x
  _ -> error "Cannot make an option column"
