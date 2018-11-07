module PersistWrap.Embedding.Rep
    ( ColumnRep(..)
    , NamedColumnRep(..)
    , NamedSchemaRep(..)
    , SchemaRep(..)
    , getSchemaRep
    ) where

import Prelude hiding ((.))

import Conkin (Tagged(..), Tuple(..))
import qualified Conkin
import Control.Category ((.))
import Data.Bijection ((:<->:), Bijection(Bi), biFrom, biTo, bimap, inverse)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Singletons (fromSing, withSingI)
import Data.Singletons.Prelude (SList, Sing(..))
import Data.Singletons.TypeLits (Sing(SSym), SSymbol, Symbol)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import PersistWrap.Conkin.Extra (findJust, getSingle, mapUncheck, noHere, singToTuple, swapOptions)
import PersistWrap.Maybe.Extra (fromJust)
import PersistWrap.Structure as Structure
import PersistWrap.Table as Table

data ColumnRep fk (x :: *) where
  UnitRep :: x -> ColumnRep fk x
  PrimRep :: SColumn c -> ColumnRep fk (Value fk c)
  FnRep :: ColumnRep fk x1 -> x1 :<->: x2 -> ColumnRep fk x2
  ForeignRep :: NamedSchemaRep fk x -> ColumnRep fk (EntityOf fk x)
  NullForeignRep :: NamedSchemaRep fk x -> ColumnRep fk (Maybe (EntityOf fk x))
  ListRep :: NamedSchemaRep fk x -> ColumnRep fk (EntityOf fk ('ListType x))
  MapRep
    :: ColumnRep fk (EntityOf fk k) -> NamedSchemaRep fk v -> ColumnRep fk (EntityOf fk ('MapType k v))

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

data SchemaRep fk structure where
  AtMostOneColumnSchema :: ColumnRep fk (EntityOf fk structure) -> SchemaRep fk structure
  ProductSchema :: Tuple nxs (NamedColumnRep fk) -> SchemaRep fk ('ProductType nxs)
  SumUnIndexedSchema
    :: Maybe (Tagged (nx ': nxs) (NamedColumnRep fk)) -- Value when all columns are null
    -> Tuple (nx ': nxs) (NamedColumnRep fk)
    -> SchemaRep fk ('SumType nx nxs)
  SumIndexedSchema :: Tuple (nx ': nxs) (NamedColumnRep fk) -> SchemaRep fk ('SumType nx nxs)

data NamedColumnRep fk nx where
  NamedColumnRep
    :: SSymbol n -> ColumnRep fk (EntityOf fk structure) -> NamedColumnRep fk '(n,structure)

data NamedSchemaRep fk x = NamedSchemaRep Text (SchemaRep fk x)

getSchemaRep :: Text -> SStructure structure -> NamedSchemaRep fk structure
getSchemaRep prefix = NamedSchemaRep prefix . either AtMostOneColumnSchema id . buildRep prefix

type BuildRepResult fk structure = Either (ColumnRep fk (EntityOf fk structure)) (SchemaRep fk structure)

biProduct :: Tuple xs (EntityOfSnd fk) :<->: EntityOf fk ( 'ProductType xs)
biProduct = Bi Product (\(Product x) -> x)

buildProductRep
  :: forall fk xs
   . Text
  -> SList (xs :: [(Symbol, Structure Symbol)])
  -> BuildRepResult fk ( 'ProductType xs)
buildProductRep prefix xs = case nonUnitsCount of
  0 -> Left $ UnitRep $ Product $ Conkin.fmap (fromJust . extractUnit) colReps
  1 -> Left $ case buildSingleProductCol colReps of
    SingleCol cr fn -> bimapCol (biProduct . fn) cr
  _ -> Right $ ProductSchema colReps
  where
    colReps       = buildColReps prefix xs
    nonUnitsCount = length $ filter not $ mapUncheck isUnit colReps

isUnit :: NamedColumnRep fk x -> Bool
isUnit = \case
  NamedColumnRep _ UnitRep{} -> True
  _                          -> False

getUnitOption :: forall fk x . NamedColumnRep fk x -> Maybe (NamedColumnRep fk x)
getUnitOption (NamedColumnRep name r) = NamedColumnRep name <$> go r
  where
    go :: forall x' . ColumnRep fk x' -> Maybe (ColumnRep fk x')
    go = \case
      UnitRep x             -> Just $ UnitRep x
      PrimRep (SColumn n _) -> case n of
        SFalse -> Nothing
        STrue  -> Just (UnitRep (N Nothing))
      FnRep cr fn      -> bimapCol fn <$> go cr
      ForeignRep{}     -> Nothing
      NullForeignRep{} -> Just $ UnitRep Nothing
      c@ListRep{}      -> Just c
      c@MapRep{}       -> Just c

hasUnitOption :: NamedColumnRep fk x -> Bool
hasUnitOption = isJust . getUnitOption

extractUnit :: NamedColumnRep fk x -> Maybe (EntityOfSnd fk x)
extractUnit = \case
  NamedColumnRep name (UnitRep u) -> Just $ EntityOfSnd name u
  _                               -> Nothing

addName :: Text -> SSymbol name -> Text
addName prefix name = prefix <> "_" <> fromSing name

data SingleCol fk xs where
  SingleCol :: ColumnRep fk x -> (x :<->: Tuple xs (EntityOfSnd fk)) -> SingleCol fk xs

buildSingleProductCol :: HasCallStack => Tuple xs (NamedColumnRep fk) -> SingleCol fk xs
buildSingleProductCol = \case
  Nil -> error "No column"
  NamedColumnRep name (UnitRep x) `Cons` xs -> case buildSingleProductCol xs of
    SingleCol cr fn ->
      SingleCol cr (Bi (EntityOfSnd name x `Cons`) (\(_ `Cons` rest) -> rest) . fn)
  NamedColumnRep name x `Cons` xs ->
    let rest = Conkin.fmap (fromJust . extractUnit) xs
    in  SingleCol x (Bi (\v -> EntityOfSnd name v `Cons` rest) (\(EntityOfSnd _ v `Cons` _) -> v))

buildColRep :: Text -> SStructure structure -> ColumnRep fk (EntityOf fk structure)
buildColRep prefix structure = case buildRep prefix structure of
  Left  cr -> cr
  Right sr -> ForeignRep (NamedSchemaRep prefix sr)

buildColReps :: Text -> SList (xs :: [(Symbol, Structure Symbol)]) -> Tuple xs (NamedColumnRep fk)
buildColReps prefix xs = Conkin.fmap
  (\(STuple2 name x) -> NamedColumnRep name (buildColRep (addName prefix name) x))
  (singToTuple xs)

buildSumRep
  :: forall x xs fk
   . Text
  -> SList (x ': xs :: [(Symbol, Structure Symbol)])
  -> BuildRepResult fk ( 'SumType x xs)
buildSumRep prefix xs = case colReps of
  NamedColumnRep name cr `Cons` Nil ->
    Left $ bimapCol (biSum . Bi Here getSingle . Bi (EntityOfSnd name) (\(EntityOfSnd _ v) -> v)) cr
  _ -> case (unitsCount, nonUnitsCount) of
    (_, 0)                   -> Left $ buildEnumCol colReps
    (1, 1)                   -> Left $ buildOptionCol colReps
    (_, _) | unitsCount <= 1 -> Right $ SumUnIndexedSchema (findJust getUnitOption colReps) colReps
    (_, _)                   -> Right $ SumIndexedSchema colReps
  where
    colReps       = buildColReps prefix xs
    unitsCount    = length $ filter id $ mapUncheck hasUnitOption colReps
    nonUnitsCount = length $ filter not $ mapUncheck isUnit colReps

biSum :: Tagged (x ': xs) (EntityOfSnd fk) :<->: EntityOf fk ( 'SumType x xs)
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
            Here  Proxy -> Here (EntityOfSnd name v)
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
  -> ColumnRep fk (EntityOf fk ( 'SumType x xs))
buildEnumCol xs = case buildEnumResult xs of
  EnumResult names conversion -> case names of
    SNil                      -> error "no names?"
    nameHead `SCons` nameTail -> FnRep
      (PrimRep (SColumn SFalse (SEnum nameHead nameTail)))
      (biSum . conversion . Bi (\(EV (EnumVal x)) -> x) (EV . EnumVal) . inverse biV)

makeOption
  :: HasCallStack
  => EntityOfSnd fk nx
  -> NamedColumnRep fk ny
  -> ColumnRep fk (EntityOf fk ( 'SumType nx '[ny]))
makeOption def (NamedColumnRep name cr) =
  bimapCol
      (biSum . Bi
        (maybe (Here def) (There . Here . EntityOfSnd name))
        (\case
          Here  _                              -> Nothing
          There (getSingle -> EntityOfSnd _ x) -> Just x
        )
      )
    $ makeNullable cr

swapCases
  :: ColumnRep fk (EntityOf fk ( 'SumType nx '[ny]))
  -> ColumnRep fk (EntityOf fk ( 'SumType ny '[nx]))
swapCases = bimapCol (biSum . Bi swapOptions swapOptions . inverse biSum)

buildOptionCol
  :: HasCallStack
  => Tuple (x ': xs) (NamedColumnRep fk)
  -> ColumnRep fk (EntityOf fk ( 'SumType x xs))
buildOptionCol = \case
  (NamedColumnRep namex (UnitRep x) `Cons` y `Cons` Nil) -> makeOption (EntityOfSnd namex x) y
  (x `Cons` NamedColumnRep namey (UnitRep y) `Cons` Nil) ->
    swapCases $ makeOption (EntityOfSnd namey y) x
  _ -> error "Cannot make an option column"

buildRep :: Text -> SStructure structure -> BuildRepResult fk structure
buildRep prefix = \case
  SPrimitive pn -> Left $ FnRep
    (PrimRep (SColumn SFalse (SPrim pn)))
    (Bi (\(PV x) -> Structure.Prim x) (\(Structure.Prim x) -> PV x) . inverse biV)
  SForeign name -> Left $ FnRep
    (PrimRep (SColumn SFalse (SForeignKey name)))
    (Bi (\(FKV fk) -> withSingI name Structure.ForeignKey fk) (\(Structure.ForeignKey fk) -> FKV fk)
    . inverse biV
    )
  SUnitType        -> Left $ UnitRep Unit
  SProductType nxs -> buildProductRep prefix nxs
  SSumType nx nxs  -> buildSumRep prefix (nx `SCons` nxs)
  SListType x      -> Left $ ListRep $ getSchemaRep prefix x
  SMapType k v ->
    Left $ MapRep (buildColRep (addName prefix (SSym @"_key")) k) $ getSchemaRep prefix v
