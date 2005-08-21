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


data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveX;    moveX    = proxy::Proxy MoveX
data Print;    print    = proxy::Proxy Print
data GetColor; getColor = proxy::Proxy GetColor


-- Newtypes for nominal type distinctions

newtype PP x = PP x -- Printable points
newtype CP x = CP x -- Colored points
newtype SP x = SP x -- "Special" points


-- A class for newtypes

class Nomination f
 where
  wrap   :: x -> f x
  unwrap :: f x -> x


-- Wrapping and unwrapping for PP, CP, SP

instance Nomination PP where wrap = PP; unwrap (PP x) = x
instance Nomination CP where wrap = CP; unwrap (CP x) = x
instance Nomination SP where wrap = SP; unwrap (SP x) = x


-- The familiar printable points but nominal this time

printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO $ PP -- Nominal!
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveX     .=. (\d -> modifyIORef x (+d))
       .*. print    .=. ((s # getX ) >>= Prelude.print)
       .*. emptyRecord


-- Colored points exercising overriding and extension

colored_point x_init (color::String) self =
   do
      super <- printable_point x_init self
      returnIO $ CP -- Nominal!
        $  print .=. ( do  putStr "so far - "; super # print
                           putStr "color  - "; Prelude.print color )
       .<. getColor .=. (returnIO color)
       .*. unwrap super -- Remove wrapper!


-- Special points that are structurally equal to PP

special_point x_init self = 
   do
      super <- printable_point x_init self
      returnIO $ SP $ unwrap super


-- For method look-up

instance (HasField l x v, Nomination f) => HasField l (f x) v
 where hLookupByLabel l o = unwrap o # l


-- These versions require explicit cast

printPP (aPP::PP x) = aPP # print
printCP (aCP::CP x) = aCP # print
printSP (aSP::SP x) = aSP # print


-- These versions upcast by themselves

printPP' o = let (aPP::PP x) = upCast o in aPP # print
printCP' o = let (aCP::CP x) = upCast o in aCP # print
printSP' o = let (aSP::SP x) = upCast o in aSP # print


-- The presentation of the nominal inheritance hierarchy

class (Nomination f, Nomination g) => NSubtype f g
 where upCast :: f x -> g x
       upCast = wrap . unwrap
instance Nomination f => NSubtype f f -- subtyping is reflexive
instance NSubtype CP PP -- colored points are printable points
instance NSubtype SP PP -- special points are printable points


-- Time to demo

main = do
           -- Some sample objects
           aPP <- mfix $ printable_point 5
           aCP <- mfix $ colored_point 5 "red"
           aSP <- mfix $ special_point 42

           -- Method invocations based on HasField
           aPP # print
           aCP # print
           aSP # print

           -- Nominal subtyping with explicit up-cast
	   printPP aPP
           -- printPP aCP -- Error! Up-cast needed.
           printPP (upCast aCP)
           printPP (upCast aSP)

           -- Nominal subtyping with implicit up-cast
	   printPP' aPP
           printPP' aCP -- No need to up-cast.
           printPP' aSP -- No need to up-cast.

           -- Nominal not equal structural subtyping
	   printSP aSP
	   -- printSP aPP -- Error! Nominal type different
