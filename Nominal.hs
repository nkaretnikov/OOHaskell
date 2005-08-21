{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

Illustration of nominal subtyping.

-}


module Nominal where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)


-- Some labels

data Print; print = proxy::Proxy Print
data Void;  void  = proxy::Proxy Void


-- Nominal type protectors

newtype A x = A { unA :: x }
newtype B x = B { unB :: x }
newtype C x = C { unC :: x }


-- Base class

a = returnIO $ A $
         print .=. putStr "a"
     .*. emptyRecord


-- Subclass with overriding

b = do 
       anA <- a
       returnIO $ B $
            print .=. do { anA # print; putStr "b" }
        .<. unA anA


-- Subclass with overriding and extension

c = do 
       anA <- a
       returnIO $ C $
            print .=. do { anA # print; putStr "c" }
        .<. void  .=. returnIO ()
        .*. unA anA


-- For structural subtype polymorphism

instance HasField l x v => HasField l (A x) v
 where hLookupByLabel l (A x) = x # l

instance HasField l x v => HasField l (B x) v
 where hLookupByLabel l (B x) = x # l

instance HasField l x v => HasField l (C x) v
 where hLookupByLabel l (C x) = x # l


-- This version requires explicit cast

printA (anA::A x) = anA # print


-- This version upcasts by itself

printA' o = let (anA::A x) = upCast o in anA # print


-- The presentation of the nominal inheritance hierarchy

class UpCast f g where upCast :: f x -> g x
instance UpCast f f where upCast = id
instance UpCast B A where upCast (B x) = A x
instance UpCast C A where upCast (C x) = A x


main = do
           anA <- a
           anB <- b
           anC <- c
           anA # print
           putStr "\n"
           anB # print
           putStr "\n"
           anC # print
           putStr "\n"
	   printA anA
           putStr "\n"
           -- printA anB -- needs up-cast
           printA (upCast anB)
           putStr "\n"
           printA (upCast anC)
           putStr "\n"
	   printA' anA
           putStr "\n"
	   printA' anB -- does not need up-cast
           putStr "\n"
	   printA' anC -- does not need up-cast
           putStr "\n"
