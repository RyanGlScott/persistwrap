{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Structure.Primitives where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)

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
    | PrimNull
    | PrimObjectId
    | PrimDbSpecific
    deriving Show
  |])

$(singDecideInstance ''PrimName)
$(singEqInstance ''PrimName)
$(singOrdInstance ''PrimName)

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
  PrimType 'PrimNull = ()
  PrimType 'PrimObjectId = ByteString
  PrimType 'PrimDbSpecific = ByteString

deriveConstraint
  :: forall c p y
   . ( c Text
     , c ByteString
     , c Int64
     , c Double
     , c Rational
     , c Bool
     , c Day
     , c TimeOfDay
     , c UTCTime
     , c ()
     )
  => SPrimName p
  -> (c (PrimType p) => y)
  -> y
deriveConstraint p cont = $(sCases ''PrimName [| p |] [| cont |])

data SingPrim = forall (p :: PrimName). SingPrim (SPrimName p) (PrimType p)
