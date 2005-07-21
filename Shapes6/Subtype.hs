{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Subtype where

import GHC.IOBase
import Foreign


-- Operations for observation and mutation

infix 7 .?.   -- observe non-monadically
infix 7 .!.   -- mutate  non-monadically
infix 7 .?>.  -- observe     monadically
infix 7 .!>.  -- mutate      monadically


-- The general class subtyping

class Subtype a b
 where
  (.?.)  :: (b -> r) -> a -> r
  (.!.)  :: (b -> b) -> a -> a
  (.?>.) :: (b -> IO r) -> a -> IO r
  (.!>.) :: (b -> IO b) -> a -> IO a
  f .?. x = unsafePerformIO $ (returnIO . f) .?>. x
  f .!. x = unsafePerformIO $ (returnIO . f) .!>. x


-- We presume that subtyping is reflexive.

instance Subtype a a
 where
  f .?>. x  = f x
  f .!>. x  = f x
