{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Primitives where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Singletons.TH
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import qualified Language.Haskell.TH.Lift as TH
import Language.Haskell.TH.PromotedLift (PromotedLift(..))
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)
import Test.QuickCheck.Instances ()

-- TODO Add list and map. Careful about the Arbitrary instances.
$(singletons [d|
  data PrimName
    = PrimText
    | PrimByteString
    | PrimInt64
    | PrimDouble
    | PrimRational
    | PrimBool
    | PrimDay
    | PrimTimeOfDay
    | PrimUTCTime
    deriving (Eq, Ord, Show, Bounded, Enum)
  |])

type family PrimType p where
  PrimType 'PrimText = Text
  PrimType 'PrimByteString = ByteString
  PrimType 'PrimInt64 = Int64
  PrimType 'PrimDouble = Double
  PrimType 'PrimRational = Rational
  PrimType 'PrimBool = Bool
  PrimType 'PrimDay = Day
  PrimType 'PrimTimeOfDay = TimeOfDay
  PrimType 'PrimUTCTime = UTCTime

deriveConstraint
  :: forall c p y
   . (c Text, c ByteString, c Int64, c Double, c Rational, c Bool, c Day, c TimeOfDay, c UTCTime)
  => SPrimName p
  -> (c (PrimType p) => y)
  -> y
deriveConstraint p cont = case p of
  SPrimText       -> cont
  SPrimByteString -> cont
  SPrimInt64      -> cont
  SPrimDouble     -> cont
  SPrimRational   -> cont
  SPrimBool       -> cont
  SPrimDay        -> cont
  SPrimTimeOfDay  -> cont
  SPrimUTCTime    -> cont

data SingPrim = forall (p :: PrimName). SingPrim (SPrimName p) (PrimType p)
instance Eq SingPrim where
  (==) (SingPrim sl pl) (SingPrim sr pr) = case sl %~ sr of
    Proved Refl -> deriveConstraint @Eq sl (==) pl pr
    Disproved{} -> False
instance Ord SingPrim where
  compare (SingPrim sl pl) (SingPrim sr pr) = case sl %~ sr of
    Proved Refl -> deriveConstraint @Ord sl compare pl pr
    Disproved{} -> compare (fromSing sl) (fromSing sr)
instance Show SingPrim where
  showsPrec d (SingPrim s p) =
    showParen (d > 10)
      $ showString "SingPrim "
      . showsPrec 11 s
      . showString " "
      . deriveConstraint @Show s showsPrec 11 p

instance Arbitrary PrimName where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SingPrim where
  arbitrary = do
    pn <- arbitrary
    withSomeSing pn $ \spn -> SingPrim spn <$> deriveConstraint @Arbitrary spn arbitrary
  shrink (SingPrim spn x) = map (SingPrim spn) $ deriveConstraint @Arbitrary spn shrink x

deriving instance TH.Lift PrimName
instance PromotedLift PrimName where
  promotedLift = \case
    PrimText       -> [t| 'PrimText |]
    PrimByteString -> [t| 'PrimByteString |]
    PrimInt64      -> [t| 'PrimInt64 |]
    PrimDouble     -> [t| 'PrimDouble |]
    PrimRational   -> [t| 'PrimRational |]
    PrimBool       -> [t| 'PrimBool |]
    PrimDay        -> [t| 'PrimDay |]
    PrimTimeOfDay  -> [t| 'PrimTimeOfDay |]
    PrimUTCTime    -> [t| 'PrimUTCTime |]
