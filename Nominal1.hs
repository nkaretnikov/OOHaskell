{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

Illustration of nominal subtyping and transitive closure.

-}


module Nominal1 where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)


data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveX;    moveX    = proxy::Proxy MoveX
data Print;    print    = proxy::Proxy Print
data PrintInt; printi   = proxy::Proxy PrintInt
data GetColor; getColor = proxy::Proxy GetColor
data GetMass;  getMass  = proxy::Proxy GetMass


-- Newtypes for nominal type distinctions

data PP  = PP  -- Printable points
data CP  = CP  -- Colored points
data MCP = MCP -- Massive Colored points
data SP  = SP  -- "Special" points

newtype N label rec = N rec

-- A class for newtypes

class Nomination f
 where
  wrap   :: x -> N f x
  wrap x = N x
  unwrap :: N f x -> x
  unwrap (N x) = x


-- Wrapping and unwrapping for PP, CP, SP, MCP

instance Nomination PP
instance Nomination CP
instance Nomination MCP
instance Nomination SP


asnp :: nt -> x -> N nt x
asnp nt x = N x

-- The familiar printable points but nominal this time

printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO $ asnp PP -- Nominal!
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveX    .=. (\d -> modifyIORef x (+d))
       .*. printi   .=. ((s # getX ) >>= Prelude.print)
       .*. print    .=. s # printi
       .*. emptyRecord


-- Colored points exercising overriding and extension

colored_point x_init (color::String) self =
   do
      super <- printable_point x_init self
      returnIO $ asnp CP -- Nominal!
        $  printi .=. ( do  putStr "so far - "; super # printi
                            putStr "color  - "; Prelude.print color )
       .<. getColor .=. (returnIO color)
       .*. unwrap super -- Remove wrapper!


-- Massive Colored points exercising overriding and extension

mcolored_point x_init color (mass::Float) self =
   do
      super <- colored_point x_init color self
      returnIO $ asnp MCP -- Nominal!
        $  printi .=. ( do  putStr "so far - "; super # printi
                            putStr "mass  - "; Prelude.print mass )
       .<. getMass .=. (returnIO mass)
       .*. unwrap super -- Remove wrapper!

-- Special points that are structurally equal to PP

special_point x_init self = 
   do
      super <- printable_point x_init self
      returnIO $ asnp SP $ unwrap super


-- For method look-up. It should not overlap with anything else

instance (HasField l x v, Nomination f) => HasField l (N f x) v
 where hLookupByLabel l o = unwrap o # l


-- These versions require explicit cast

printPP (aPP::N PP x) = aPP # print
printCP (aCP::N CP x) = aCP # print
printSP (aSP::N SP x) = aSP # print


-- These versions upcast by themselves

printPP' o = let (aPP::N PP x) = upCast o in aPP # print
printCP' o = let (aCP::N CP x) = upCast o in aCP # print
printSP' o = let (aSP::N SP x) = upCast o in aSP # print


-- The presentation of the nominal inheritance hierarchy

-- Define which nominal type is a super for which type
-- For multiple inheritance, we should define the list of parents.
-- That is explored in the message on the Haskell mailing list ca end 2003

class NSuper sub super | sub -> super

instance NSuper CP PP -- colored points are printable points
instance NSuper SP PP -- special points are printable points
instance NSuper MCP CP -- massive colored points are colored points

-- Now we compute the transitive closure from a nominal type
-- to any of its ancestors

class (Nomination f, Nomination g) => NSubclass f g
 where upCast :: N f x -> N g x
       upCast = wrap . unwrap

instance (Nomination f, Nomination g, TypeEq f g bool,
	  NSubclass' bool f g) 
    => NSubclass f g
class (Nomination f, Nomination g) => NSubclass' bool f g
instance Nomination f => NSubclass' HTrue f f  -- subclassing is reflexive
instance (Nomination f, Nomination g, Nomination g',
	  NSuper f g', TypeEq g' g bool,
	  NSubclass'' bool f g)
    => NSubclass' HFalse f g

class (Nomination f, Nomination g) => NSubclass'' bool f g

instance (Nomination f, Nomination g) => NSubclass'' HTrue f g
instance (Nomination f, Nomination g, Nomination g',
	  NSuper f g', NSubclass g' g) => NSubclass'' HFalse f g

-- Time to demo

castto x (np::np) = let (o::N np x) = upCast x in o

main = do
           -- Some sample objects
           aPP <- mfix $ printable_point 5
           aCP <- mfix $ colored_point 5 "red"
           aMP <- mfix $ mcolored_point 7 "red" 14.7
           aSP <- mfix $ special_point 42

           putStrLn "Method invocations based on HasField"
           aPP # print
           aCP # print
           aMP # print
           aSP # print

           putStrLn "Nominal subtyping with explicit up-cast"
	   printPP aPP
           -- printPP aCP -- Error! Up-cast needed.
           printPP (upCast aCP)
           printPP (upCast aSP)
           printPP (upCast aMP)

           putStrLn "Nominal subtyping with implicit up-cast"
	   printPP' aPP
           printPP' aCP -- No need to up-cast.
           printPP' aSP -- No need to up-cast.
           printPP' aMP -- No need to up-cast.

           printPP' aMP -- No need to up-cast.

           -- moveX must be inherited from the remote parent
	   putStrLn "    after moving MP..."
           (castto aMP PP) # moveX $ 10
           printPP' aMP -- No need to up-cast.



           -- Nominal not equal structural subtyping
	   printSP aSP
	   -- printSP aPP -- Error! Nominal type different
