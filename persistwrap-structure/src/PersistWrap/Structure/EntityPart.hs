{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Structure.EntityPart
    ( EntityPart(..)
    , GStructureOf
    ) where

import Conkin (Tagged(..), Tuple(..))
import Control.Arrow ((***))
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Singletons.Prelude (type (++), Symbol)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Singletons
import Data.Singletons.Prelude.Enum (Succ, sSucc)
import Data.Singletons.Prelude.List (SList, Sing(SCons, SNil))
import Data.Singletons.Prelude.Maybe (FromMaybeSym0)
import Data.Singletons.Prelude.Show (Show_)
import Data.Singletons.Prelude.Tuple (Sing(STuple2))
import Data.Singletons.TypeLits (Nat, SNat)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import GHC.Generics as Generics

import Consin
import PersistWrap.Primitives
import PersistWrap.Structure.EntityOf
import PersistWrap.Structure.Singletons (First)
import PersistWrap.Structure.Type

class (SingI (StructureOf t)) => EntityPart (fk :: Symbol -> *) t where
  type StructureOf t :: Structure Symbol
  fromEntity :: EntityOf fk (StructureOf t) -> t
  default fromEntity :: (Generic t, GEntityPart fk (Rep t), StructureOf t ~ GStructureOf (Rep t))
    => EntityOf fk (StructureOf t) -> t
  fromEntity = genericFromEntity @fk
  toEntity :: t -> EntityOf fk (StructureOf t)
  default toEntity :: (Generic t, GEntityPart fk (Rep t), StructureOf t ~ GStructureOf (Rep t))
    => t -> EntityOf fk (StructureOf t)
  toEntity = genericToEntity @fk

instance {-# OVERLAPPABLE #-} SingI name => EntityPart fk (fk name) where
  type StructureOf (fk name) = 'Foreign name
  fromEntity (ForeignKey x) = x
  toEntity = ForeignKey

instance EntityPart fk Text where
  type StructureOf Text = 'Primitive 'PrimText
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk ByteString where
  type StructureOf ByteString = 'Primitive 'PrimByteString
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk Int64 where
  type StructureOf Int64 = 'Primitive 'PrimInt64
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk Double where
  type StructureOf Double = 'Primitive 'PrimDouble
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk Rational where
  type StructureOf Rational = 'Primitive 'PrimRational
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk Bool where
  type StructureOf Bool = 'Primitive 'PrimBool
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk Day where
  type StructureOf Day = 'Primitive 'PrimDay
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk TimeOfDay where
  type StructureOf TimeOfDay = 'Primitive 'PrimTimeOfDay
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk UTCTime where
  type StructureOf UTCTime = 'Primitive 'PrimUTCTime
  fromEntity (Prim x) = x
  toEntity = Prim
instance EntityPart fk Int where
  type StructureOf Int = StructureOf Int64
  fromEntity x = fromIntegral (fromEntity x :: Int64)
  toEntity x = toEntity (fromIntegral x :: Int64)
instance EntityPart fk Integer where
  type StructureOf Integer = StructureOf Text
  fromEntity = read . Text.unpack . fromEntity
  toEntity   = toEntity . Text.pack . show

instance EntityPart fk v => EntityPart fk [v] where
  type StructureOf [v] = 'ListType (StructureOf v)
  fromEntity (List xs) = map (fromEntity @fk) xs
  toEntity xs = List $ map (toEntity @fk) xs
instance (AlwaysS Eq fk, AlwaysS Ord fk, Ord k, EntityPart fk k, EntityPart fk v)
    => EntityPart fk (Map k v) where
  type StructureOf (Map k v) = 'MapType (StructureOf k) (StructureOf v)
  fromEntity (Map fk) = Map.fromList $ map (fromEntity @fk *** fromEntity @fk) $ Map.toList fk
  toEntity fk = Map $ Map.fromList $ map (toEntity @fk *** toEntity @fk) $ Map.toList fk
instance (Ord k, EntityPart fk k) => EntityPart fk (Set k) where
  type StructureOf (Set k) = 'ListType (StructureOf k)
  fromEntity (List s) = Set.fromList $ map (fromEntity @fk) s
  toEntity s = List $ map (toEntity @fk) $ Set.toList s

instance SingI structure => EntityPart fk (EntityOf fk structure) where
  type StructureOf (EntityOf fk structure) = structure
  fromEntity = id
  toEntity   = id

class SingI (GStructureOf f) => GEntityPart (fk :: Symbol -> *) f where
  type GStructureOf f :: Structure Symbol
  gFromEntity :: EntityOf fk (GStructureOf f) -> f x
  gToEntity :: f x -> EntityOf fk (GStructureOf f)

instance GEntityPart fk U1 where
  type GStructureOf U1 = 'UnitType
  gFromEntity Unit = U1
  gToEntity U1 = Unit

class SingI (GenericConsTail f) => GenericConsPart (fk :: Symbol -> *) f where
  type GenericConsHead f :: (Symbol, Structure Symbol)
  type GenericConsTail f :: [(Symbol, Structure Symbol)]
  fromTag :: Tagged (GenericConsList f) (EntityOfSnd fk) -> f x
  toTag :: f x -> Tagged (GenericConsList f) (EntityOfSnd fk)

type GenericConsList f = GenericConsHead f ': GenericConsTail f

instance (GEntityPart fk a) => GenericConsPart fk (C1 ('MetaCons na fa sa) a) where
  type GenericConsHead (C1 ('MetaCons na fa sa) a) = '(na, GStructureOf a)
  type GenericConsTail (C1 ('MetaCons na fa sa) a) = '[]
  fromTag = \case
    Here  (EntityOfSnd x) -> M1 $ gFromEntity @fk x
    There x               -> case x of
  toTag (M1 x) = Here $ EntityOfSnd (gToEntity @fk x)

instance
    ( GenericConsPart fk a
    , GenericConsPart fk b
    , SingI (GenericConsHead a)
    , SingI (GenericConsTail a ++ GenericConsList b)
    )
    => GenericConsPart fk (a :+: b) where
  type GenericConsHead (a :+: b) = GenericConsHead a
  type GenericConsTail (a :+: b) = GenericConsTail a ++ GenericConsList b
  fromTag x = case pickSide x of
    Left  l -> L1 $ fromTag @fk l
    Right r -> R1 $ fromTag @fk r
  toTag = \case
    L1 l -> leftTag @(GenericConsList a) @(GenericConsList b) $ toTag @fk l
    R1 r -> rightTag @(GenericConsList a) @(GenericConsList b) $ toTag @fk r

instance (SingI (GenericConsHead a), GenericConsPart fk (a :+: b)) => GEntityPart fk (a :+: b) where
  type GStructureOf (a :+: b) = 'SumType (GenericConsHead (a :+: b) ':| GenericConsTail (a :+: b))
  gFromEntity (Sum x) = fromTag @fk x
  gToEntity x = Sum $ toTag @fk x

data EntityMNOfSnd fk x where
  EntityMNOfSnd :: forall (fk :: Symbol -> *) (sym :: Maybe Symbol) (struct :: Structure Symbol).
    EntityOf fk struct -> EntityMNOfSnd fk '( sym , struct )

type family FillInDefaultNamesFrom (i :: Nat) (xs :: [(Maybe Symbol, Structure Symbol)])
    :: [(Symbol, Structure Symbol)] where
  FillInDefaultNamesFrom i '[] = '[]
  FillInDefaultNamesFrom i ( x ': xs )
    = First (Apply FromMaybeSym0 (Show_ i)) x ': FillInDefaultNamesFrom (Succ i) xs

type FillInDefaultNames xs = FillInDefaultNamesFrom 1 xs

fillInDefaultNames
  :: forall xs fk . Tuple xs (EntityMNOfSnd fk) -> Tuple (FillInDefaultNames xs) (EntityOfSnd fk)
fillInDefaultNames = go $ sing @1
  where
    go
      :: forall i xs'
       . SNat i
      -> Tuple xs' (EntityMNOfSnd fk)
      -> Tuple (FillInDefaultNamesFrom i xs') (EntityOfSnd fk)
    go singi = \case
      Nil                       -> Nil
      EntityMNOfSnd x `Cons` xs -> EntityOfSnd x `Cons` go (sSucc singi) xs

stripOutDefaultNames
  :: forall (xs :: [(Maybe Symbol, Structure Symbol)]) (fk :: Symbol -> *)
   . SingI xs
  => Tuple (FillInDefaultNames xs) (EntityOfSnd fk)
  -> Tuple xs (EntityMNOfSnd fk)
stripOutDefaultNames = go (sing @1) sing
  where
    go
      :: forall i xs'
       . SNat i
      -> SList xs'
      -> Tuple (FillInDefaultNamesFrom i xs') (EntityOfSnd fk)
      -> Tuple xs' (EntityMNOfSnd fk)
    go si = \case
      SNil -> const Nil
      STuple2 _ _ `SCons` singxs ->
        \(EntityOfSnd structx `Cons` xs) -> EntityMNOfSnd structx `Cons` go (sSucc si) singxs xs

class SingI (GenericRecList f) => GenericRecPart (fk :: Symbol -> *) f where
  type GenericRecList f :: [(Maybe Symbol, Structure Symbol)]
  fromField :: Tuple (GenericRecList f) (EntityMNOfSnd fk) -> f x
  toField :: f x -> Tuple (GenericRecList f) (EntityMNOfSnd fk)

instance (GEntityPart fk a, SingI mn)
    => GenericRecPart fk (S1 ('MetaSel mn su ss ds) a) where
  type GenericRecList (S1 ('MetaSel mn su ss ds) a) = '[ '( mn , GStructureOf a ) ]
  fromField (EntityMNOfSnd x `Cons` Nil) = M1 $ gFromEntity @fk x
  toField (M1 x) = EntityMNOfSnd (gToEntity @fk x) `Cons` Nil

instance
    ( GenericRecPart fk a
    , GenericRecPart fk b
    , SingI (GenericRecList a ++ GenericRecList b)
    )
    => GenericRecPart fk (a :*: b) where
  type GenericRecList (a :*: b) = GenericRecList a ++ GenericRecList b
  fromField x =
    uncurry (:*:)
      . (fromField @fk *** fromField @fk)
      $ splitTuple @(GenericRecList a) @(GenericRecList b) x
  toField (x :*: y) = toField @fk x ++& toField @fk y

instance (SingI (FillInDefaultNames (GenericRecList (a :*: b))), GenericRecPart fk (a :*: b))
    => GEntityPart fk (a :*: b) where
  type GStructureOf (a :*: b) = 'ProductType (FillInDefaultNames (GenericRecList (a :*: b)))
  gFromEntity (Product x) = fromField @fk (stripOutDefaultNames x)
  gToEntity x = Product $ fillInDefaultNames $ toField @fk x

instance GEntityPart fk a => GEntityPart fk (M1 i m a) where
  type GStructureOf (M1 i m a) = GStructureOf a
  gFromEntity = M1 . gFromEntity @fk
  gToEntity   = gToEntity @fk . unM1

instance EntityPart fk a => GEntityPart fk (K1 i a) where
  type GStructureOf (K1 i a) = StructureOf a
  gFromEntity = K1 . fromEntity @fk
  gToEntity   = toEntity @fk . unK1

genericToEntity
  :: forall fk a . (Generic a, GEntityPart fk (Rep a)) => a -> EntityOf fk (GStructureOf (Rep a))
genericToEntity = gToEntity @fk . Generics.from

genericFromEntity
  :: forall fk a . (Generic a, GEntityPart fk (Rep a)) => EntityOf fk (GStructureOf (Rep a)) -> a
genericFromEntity = Generics.to . gFromEntity @fk

instance EntityPart fk () where
  type StructureOf () = 'UnitType
instance (EntityPart fk a, EntityPart fk b) => EntityPart fk (a,b) where
  type StructureOf (a,b) = 'ProductType '[ '("1", StructureOf a) , '("2", StructureOf b)]
instance (EntityPart fk a, EntityPart fk b, EntityPart fk c) => EntityPart fk (a,b,c) where
  type StructureOf (a,b,c) = 'ProductType '[ '("1", StructureOf a) , '("2", StructureOf b), '("3", StructureOf c) ]
instance (EntityPart fk a, EntityPart fk b, EntityPart fk c, EntityPart fk d) => EntityPart fk (a,b,c,d) where
  type StructureOf (a,b,c,d) = GStructureOf (Rep (a,b,c,d))
instance (EntityPart fk a, EntityPart fk b, EntityPart fk c, EntityPart fk d, EntityPart fk e) => EntityPart fk (a,b,c,d,e) where
  type StructureOf (a,b,c,d,e) = GStructureOf (Rep (a,b,c,d,e))
instance (EntityPart fk a, EntityPart fk b, EntityPart fk c, EntityPart fk d, EntityPart fk e, EntityPart fk f) => EntityPart fk (a,b,c,d,e,f) where
  type StructureOf (a,b,c,d,e,f) = GStructureOf (Rep (a,b,c,d,e,f))
instance (EntityPart fk a, EntityPart fk b, EntityPart fk c, EntityPart fk d, EntityPart fk e, EntityPart fk f, EntityPart fk g) => EntityPart fk (a,b,c,d,e,f,g) where
  type StructureOf (a,b,c,d,e,f,g) = GStructureOf (Rep (a,b,c,d,e,f,g))
instance (EntityPart fk a, EntityPart fk b) => EntityPart fk (Either a b) where
  type StructureOf (Either a b) = 'SumType ('("Left", StructureOf a) ':| '[ '("Right", StructureOf b) ])
instance EntityPart fk x => EntityPart fk (Maybe x) where
  type StructureOf (Maybe x) = 'SumType ('("Nothing", 'UnitType) ':| '[ '("Just", StructureOf x) ])
instance EntityPart fk x => EntityPart fk (Identity x) where
  type StructureOf (Identity x) = StructureOf x
