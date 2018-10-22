{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module PersistWrap.Structure.Primitives where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Singletons (sing)
import Data.Singletons.TH (singletons, singDecideInstance)
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

$(singDecideInstance ''PrimName)

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
     , c [PersistValue]
     , c [(Text, PersistValue)]
     )
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
  SPrimNull       -> cont
  SPrimList       -> cont
  SPrimMap        -> cont
  SPrimObjectId   -> cont
  SPrimDbSpecific -> cont

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
