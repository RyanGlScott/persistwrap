module PersistWrap.Functor.Extra
    ( (<&>)
    ) where

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
