{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistWrap.Table.EnumVal
    ( EnumVal(..)
    , enum
    ) where

import Conkin (Tagged(..))
import Data.Proxy (Proxy(Proxy))
import Data.Singletons (SingI, fromSing, sing)
import Data.Singletons.Prelude (type (==), SList, Sing(SCons, SNil))
import Data.Singletons.TypeLits (Symbol)
import Data.Text (Text)

newtype EnumVal (xs :: [Symbol]) = EnumVal (Tagged xs Proxy)

instance SingI xs => Show (EnumVal xs) where
  showsPrec d val =
    showParen (d > 10) $ showString "enum @" . showString (show (caseText sing val))

caseText :: SList (xs :: [Symbol]) -> EnumVal xs -> Text
caseText sxs0 (EnumVal ev0) = go sxs0 ev0
  where
    go :: forall (xs' :: [Symbol]) . SList xs' -> Tagged xs' Proxy -> Text
    go SNil t = case t of {}
    go (sx `SCons` _  ) (Here  Proxy) = fromSing sx
    go (_  `SCons` sxs) (There rest ) = go sxs rest

enum :: forall name xs . IsMember name xs => EnumVal xs
enum = EnumVal (enumTag @name)

class IsMember (name :: Symbol) (xs :: [Symbol]) where
  enumTag :: Tagged xs Proxy
instance IsMemberH (name == x) name (x ': xs) => IsMember name (x ': xs) where
  enumTag = enumh @(name == x) @name
class IsMemberH (current :: Bool) (name :: Symbol) (xs :: [Symbol]) where
  enumh :: Tagged xs Proxy
instance x ~ name => IsMemberH 'True name (x ': xs) where
  enumh = Here (Proxy @name)
instance IsMember name xs => IsMemberH 'False name (x ': xs) where
  enumh = There (enumTag @name)

instance Eq (EnumVal xs) where
  (==) (EnumVal x0) (EnumVal y0) = go x0 y0
    where
      go :: forall (xs' :: [Symbol]) . Tagged xs' Proxy -> Tagged xs' Proxy -> Bool
      go (Here  Proxy) (Here  Proxy) = True
      go (There x    ) (There y    ) = go x y
      go (Here  _    ) (There _    ) = False
      go (There _    ) (Here  _    ) = False

instance Ord (EnumVal xs) where
  compare (EnumVal x0) (EnumVal y0) = go x0 y0
    where
      go :: forall (xs' :: [Symbol]) . Tagged xs' Proxy -> Tagged xs' Proxy -> Ordering
      go (Here  Proxy) (Here  Proxy) = EQ
      go (There x    ) (There y    ) = go x y
      go (Here  _    ) (There _    ) = LT
      go (There _    ) (Here  _    ) = GT
