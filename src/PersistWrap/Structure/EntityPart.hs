{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Structure.EntityPart
    ( EntityPart(..)
    ) where

import Conkin (Tagged (..), Tuple (..))
import Control.Arrow ((***))
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Promotion.Prelude (type (++), Symbol)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Singletons
import Data.Singletons.Prelude.Enum (Succ, sSucc)
import Data.Singletons.Prelude.List (Sing (SCons, SNil))
import Data.Singletons.Prelude.Maybe (Sing (SJust, SNothing))
import Data.Singletons.Prelude.Show (Show_, sShow_)
import Data.Singletons.Prelude.Tuple (Sing (STuple2))
import Data.Singletons.TypeLits (KnownSymbol, Nat, SNat)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Void (Void)
import GHC.Generics as Generics

import PersistWrap.Conkin.Extra
import PersistWrap.Singletons.Extra
import PersistWrap.Structure.EntityOf
import PersistWrap.Structure.Primitives
import PersistWrap.Structure.Type

class SingI (StructureOf t) => EntityPart t where
  type StructureOf t :: Structure
  fromEntity :: EntityOf (StructureOf t) -> t
  default fromEntity :: (Generic t, GEntityPart (Rep t), StructureOf t ~ GStructureOf (Rep t))
    => EntityOf (StructureOf t) -> t
  fromEntity = genericFromEntity
  toEntity :: t -> EntityOf (StructureOf t)
  default toEntity :: (Generic t, GEntityPart (Rep t), StructureOf t ~ GStructureOf (Rep t))
    => t -> EntityOf (StructureOf t)
  toEntity = genericToEntity

instance EntityPart Text where
  type StructureOf Text = 'Primitive 'PrimText
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart ByteString where
  type StructureOf ByteString = 'Primitive 'PrimByteString
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart Int64 where
  type StructureOf Int64 = 'Primitive 'PrimInt64
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart Double where
  type StructureOf Double = 'Primitive 'PrimDouble
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart Rational where
  type StructureOf Rational = 'Primitive 'PrimRational
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart Bool where
  type StructureOf Bool = 'Primitive 'PrimBool
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart Day where
  type StructureOf Day = 'Primitive 'PrimDay
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart TimeOfDay where
  type StructureOf TimeOfDay = 'Primitive 'PrimTimeOfDay
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart UTCTime where
  type StructureOf UTCTime = 'Primitive 'PrimUTCTime
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart Int where
  type StructureOf Int = 'Primitive 'PrimInt64
  fromEntity (Prim x) = fromIntegral x
  toEntity = Prim . fromIntegral
instance EntityPart Integer where
  type StructureOf Integer = 'Primitive 'PrimText
  fromEntity (Prim x) = read $ Text.unpack x
  toEntity = Prim . Text.pack . show

instance EntityPart v => EntityPart [v] where
  type StructureOf [v] = 'ListType (StructureOf v)
  fromEntity (List xs) = map fromEntity xs
  toEntity xs = List $ map toEntity xs
instance (Ord k, EntityMapKey k, EntityPart v) => EntityPart (Map k v) where
  type StructureOf (Map k v) = 'MapType (StructureOf v)
  fromEntity (Map m) = Map.fromList . map (fromText *** fromEntity) . Map.toList $ m
  toEntity m = Map $ Map.fromList . map (toText *** toEntity) . Map.toList $ m
instance (Ord k, EntityPart k) => EntityPart (Set k) where
  type StructureOf (Set k) = 'ListType (StructureOf k)
  fromEntity (List s) = Set.fromList $ map fromEntity s
  toEntity s = List $ map toEntity $ Set.toList s

instance SingI structure => EntityPart (EntityOf structure) where
  type StructureOf (EntityOf structure) = structure
  fromEntity = id
  toEntity = id

class EntityPart k => EntityMapKey k where
  fromText :: Text -> k
  default fromText :: Coercible Text k => Text -> k
  fromText = coerce
  toText :: k -> Text
  default toText :: Coercible Text k => k -> Text
  toText = coerce

instance EntityMapKey Text

class SingI (GStructureOf f) => GEntityPart f where
  type GStructureOf f :: Structure
  gFromEntity :: EntityOf (GStructureOf f) -> f x
  gToEntity :: f x -> EntityOf (GStructureOf f)

instance GEntityPart V1 where
  type GStructureOf V1 = 'EmptyType
  gFromEntity = \case {}
  gToEntity = \case {}
instance GEntityPart U1 where
  type GStructureOf U1 = 'UnitType
  gFromEntity Unit = U1
  gToEntity U1 = Unit

class SingI (GenericConsList f) => GenericConsPart f where
  type GenericConsList f :: [(Symbol, Structure)]
  fromTag :: Tagged (GenericConsList f) EntityOfSnd -> f x
  toTag :: f x -> Tagged (GenericConsList f) EntityOfSnd

instance (GEntityPart a, KnownSymbol na)
    => GenericConsPart (C1 ('MetaCons na fa sa) a) where
  type GenericConsList (C1 ('MetaCons na fa sa) a) = '[ '(na, GStructureOf a) ]
  fromTag = \case
    Here (EntityOfSnd _ x) -> M1 $ gFromEntity x
    There x -> case x of {}
  toTag (M1 x) = Here $ EntityOfSnd sing (gToEntity x)

instance
    ( GenericConsPart a
    , GenericConsPart b
    , SingI (GenericConsList a ++ GenericConsList b)
    )
    => GenericConsPart (a :+: b) where
  type GenericConsList (a :+: b) = GenericConsList a ++ GenericConsList b
  fromTag x = case pickSide x of
    Left l  -> L1 $ fromTag l
    Right r -> R1 $ fromTag r
  toTag = \case
    L1 l -> leftTag @(GenericConsList a) @(GenericConsList b) $ toTag l
    R1 r -> rightTag @(GenericConsList a) @(GenericConsList b) $ toTag r

instance GenericConsPart (a :+: b) => GEntityPart (a :+: b) where
  type GStructureOf (a :+: b) = 'SumType (GenericConsList (a :+: b))
  gFromEntity (Sum x) = fromTag x
  gToEntity x = Sum $ toTag x

data EntityMNOfSnd x where
  EntityMNOfSnd :: forall (sym :: Maybe Symbol) (struct :: Structure).
    Sing sym -> EntityOf struct -> EntityMNOfSnd '( sym , struct )

type family FillInDefaultNamesFrom (i :: Nat) (xs :: [(Maybe Symbol, Structure)])
    :: [(Symbol, Structure)] where
  FillInDefaultNamesFrom i '[] = '[]
  FillInDefaultNamesFrom i ( x ': xs )
    = FirstFromMaybe (Show_ i) x ': FillInDefaultNamesFrom (Succ i) xs

type FillInDefaultNames xs = FillInDefaultNamesFrom 1 xs

fillInDefaultNames :: Tuple xs EntityMNOfSnd -> Tuple (FillInDefaultNames xs) EntityOfSnd
fillInDefaultNames = go (sing :: SNat 1)
 where
  go
    :: forall i xs'
     . SNat i
    -> Tuple xs' EntityMNOfSnd
    -> Tuple (FillInDefaultNamesFrom i xs') EntityOfSnd
  go singi = \case
    Nil -> Nil
    EntityMNOfSnd ssym x `Cons` xs ->
      let key = case ssym of
            SNothing   -> sShow_ singi
            SJust symx -> symx
      in  EntityOfSnd key x `Cons` go (sSucc singi) xs

stripOutDefaultNames
  :: forall (xs :: [(Maybe Symbol, Structure)])
   . SingI xs
  => Tuple (FillInDefaultNames xs) EntityOfSnd
  -> Tuple xs EntityMNOfSnd
stripOutDefaultNames = go (Proxy @1) sing
 where
  go
    :: forall i xs'
     . Proxy i
    -> Sing xs'
    -> Tuple (FillInDefaultNamesFrom i xs') EntityOfSnd
    -> Tuple xs' EntityMNOfSnd
  go _ = \case
    SNil                           -> const Nil
    STuple2 msymx _ `SCons` singxs -> \(EntityOfSnd _ structx `Cons` xs) ->
      EntityMNOfSnd msymx structx `Cons` go (Proxy @(Succ i)) singxs xs

class SingI (GenericRecList f) => GenericRecPart f where
  type GenericRecList f :: [(Maybe Symbol, Structure)]
  fromField :: Tuple (GenericRecList f) EntityMNOfSnd -> f x
  toField :: f x -> Tuple (GenericRecList f) EntityMNOfSnd

instance (GEntityPart a, SingI mn)
    => GenericRecPart (S1 ('MetaSel mn su ss ds) a) where
  type GenericRecList (S1 ('MetaSel mn su ss ds) a) = '[ '( mn , GStructureOf a ) ]
  fromField (EntityMNOfSnd _ x `Cons` Nil) = M1 $ gFromEntity x
  toField (M1 x) = EntityMNOfSnd sing (gToEntity x) `Cons` Nil

instance
    ( GenericRecPart a
    , GenericRecPart b
    , SingI (GenericRecList a ++ GenericRecList b)
    )
    => GenericRecPart (a :*: b) where
  type GenericRecList (a :*: b) = GenericRecList a ++ GenericRecList b
  fromField x = uncurry (:*:) . (fromField *** fromField)
    $ splitTuple @(GenericRecList a) @(GenericRecList b) x
  toField (x :*: y) = toField x ++& toField y

instance (SingI (FillInDefaultNames (GenericRecList (a :*: b))), GenericRecPart (a :*: b))
    => GEntityPart (a :*: b) where
  type GStructureOf (a :*: b) = 'ProductType (FillInDefaultNames (GenericRecList (a :*: b)))
  gFromEntity (Product x) = fromField (stripOutDefaultNames x)
  gToEntity x = Product $ fillInDefaultNames $ toField x

instance GEntityPart a => GEntityPart (M1 i m a) where
  type GStructureOf (M1 i m a) = GStructureOf a
  gFromEntity = M1 . gFromEntity
  gToEntity = gToEntity . unM1

instance EntityPart a => GEntityPart (K1 i a) where
  type GStructureOf (K1 i a) = StructureOf a
  gFromEntity = K1 . fromEntity
  gToEntity = toEntity . unK1

genericToEntity
  :: forall a . (Generic a, GEntityPart (Rep a)) => a -> EntityOf (GStructureOf (Rep a))
genericToEntity = gToEntity . Generics.from

genericFromEntity
  :: forall a . (Generic a, GEntityPart (Rep a)) => EntityOf (GStructureOf (Rep a)) -> a
genericFromEntity = Generics.to . gFromEntity

instance EntityPart Void where
  type StructureOf Void = 'EmptyType
instance EntityPart () where
  type StructureOf () = 'UnitType
instance (EntityPart a, EntityPart b) => EntityPart (a,b) where
  type StructureOf (a,b) = 'ProductType '[ '("1", StructureOf a) , '("2", StructureOf b)]
instance (EntityPart a, EntityPart b, EntityPart c) => EntityPart (a,b,c) where
  type StructureOf (a,b,c) = 'ProductType '[ '("1", StructureOf a) , '("2", StructureOf b), '("3", StructureOf c) ]
instance (EntityPart a, EntityPart b, EntityPart c, EntityPart d) => EntityPart (a,b,c,d) where
  type StructureOf (a,b,c,d) = GStructureOf (Rep (a,b,c,d))
instance (EntityPart a, EntityPart b, EntityPart c, EntityPart d, EntityPart e) => EntityPart (a,b,c,d,e) where
  type StructureOf (a,b,c,d,e) = GStructureOf (Rep (a,b,c,d,e))
instance (EntityPart a, EntityPart b, EntityPart c, EntityPart d, EntityPart e, EntityPart f) => EntityPart (a,b,c,d,e,f) where
  type StructureOf (a,b,c,d,e,f) = GStructureOf (Rep (a,b,c,d,e,f))
instance (EntityPart a, EntityPart b, EntityPart c, EntityPart d, EntityPart e, EntityPart f, EntityPart g) => EntityPart (a,b,c,d,e,f,g) where
  type StructureOf (a,b,c,d,e,f,g) = GStructureOf (Rep (a,b,c,d,e,f,g))
instance (EntityPart a, EntityPart b) => EntityPart (Either a b) where
  type StructureOf (Either a b) = 'SumType '[ '("Left", StructureOf a) , '("Right", StructureOf b) ]
instance EntityPart x => EntityPart (Maybe x) where
  type StructureOf (Maybe x) = 'SumType '[ '("Nothing", 'UnitType) , '("Just", StructureOf x) ]
instance EntityPart x => EntityPart (Identity x) where
  type StructureOf (Identity x) = StructureOf x
