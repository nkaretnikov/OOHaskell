{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- The concept of subtyping (or perhaps we should say: substitution).
-- The use of a two-parameter type-class is not really essential.
-- In Haskell 98, we would simply specialise this type-class per OO base-class.
-- The generic instance can be eliminated as well by specialisation.

module Subtype where

import GHC.IOBase

infix 7 .?.
infix 7 .!.

class Subtype a b where
 (.?.) :: (b -> IO r) -> a -> IO r
 (.!.) :: (b -> IO b) -> a -> IO a

instance Subtype a a where
 f .?. x = f x
 f .!. x = f x

-- We still need to many Subtype instances (transitively).
-- We would need to use type-level cast and friend to improve on that.
