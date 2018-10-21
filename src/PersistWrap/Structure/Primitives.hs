{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.Primitives where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Singletons (sing)
import Data.Singletons.TH (singletons)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Database.Persist.Types (PersistValue (..))

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
    | PrimList
    | PrimMap
    | PrimObjectId
    | PrimDbSpecific
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
  PrimType 'PrimNull = ()
  PrimType 'PrimList = [PersistValue]
  PrimType 'PrimMap = [(Text, PersistValue)]
  PrimType 'PrimObjectId = ByteString
  PrimType 'PrimDbSpecific = ByteString

data SingPrim = forall (p :: PrimName). SingPrim (SPrimName p) (PrimType p)

primitive :: PersistValue -> SingPrim
primitive = \case
  PersistText       x -> SingPrim @ 'PrimText sing x
  PersistByteString x -> SingPrim @ 'PrimByteString sing x
  PersistInt64      x -> SingPrim @ 'PrimInt64 sing x
  PersistDouble     x -> SingPrim @ 'PrimDouble sing x
  PersistRational   x -> SingPrim @ 'PrimRational sing x
  PersistBool       x -> SingPrim @ 'PrimBool sing x
  PersistDay        x -> SingPrim @ 'PrimDay sing x
  PersistTimeOfDay  x -> SingPrim @ 'PrimTimeOfDay sing x
  PersistUTCTime    x -> SingPrim @ 'PrimUTCTime sing x
  PersistNull         -> SingPrim @ 'PrimNull sing ()
  PersistList       x -> SingPrim @ 'PrimList sing x
  PersistMap        x -> SingPrim @ 'PrimMap sing x
  PersistObjectId   x -> SingPrim @ 'PrimObjectId sing x
  PersistDbSpecific x -> SingPrim @ 'PrimDbSpecific sing x
