{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

Demonstrate nominal subtyping.

-}


module NominalTest where

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


-- Nominations

instance Nomination PP
instance Nomination CP
instance Nomination MCP
instance Nomination SP


-- The familiar printable points but nominal this time

printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO $ nominate PP -- Nominal!
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
      returnIO $ nominate CP -- Nominal!
        $  printi .=. ( do  putStr "so far - "; super # printi
                            putStr "color  - "; Prelude.print color )
       .<. getColor .=. (returnIO color)
       .*. anonymize super -- Access record!


-- Massive Colored points exercising overriding and extension

mcolored_point x_init color (mass::Float) self =
   do
      super <- colored_point x_init color self
      returnIO $ nominate MCP -- Nominal!
        $  printi .=. ( do  putStr "so far - "; super # printi
                            putStr "mass  - "; Prelude.print mass )
       .<. getMass .=. (returnIO mass)
       .*. anonymize super -- Access record!


-- Special points that are structurally equal to PP

special_point x_init self = 
   do
      super <- printable_point x_init self
      returnIO $ nominate SP $ anonymize super


-- These versions require explicit cast

cast_as_p :: p -> N p x -> N p x; cast_as_p _ = id

printPP aPP = (cast_as_p PP aPP) # print
printCP aCP = (cast_as_p CP aCP) # print
printSP aSP = (cast_as_p SP aSP) # print


-- These versions upcast by themselves

printPP' o = printPP (nUpCast o)
printCP' o = printCP (nUpCast o)
printSP' o = printSP (nUpCast o)


-- Printable points don't have parents

instance Parents PP HNil


-- Colored points are printable points

instance Parents CP (HCons PP HNil) 


-- Special points are printable points
 
instance Parents SP (HCons PP HNil)


-- Massive colored points are colored points

instance Parents MCP (HCons CP HNil)


-- Time to demo

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
           printPP (nUpCast aCP)
           printPP (nUpCast aSP)
           printPP (nUpCast aMP)

           putStrLn "Nominal subtyping with implicit up-cast"
	   printPP' aPP
           printPP' aCP -- No need to up-cast.
           printPP' aSP -- No need to up-cast.
           printPP' aMP -- No need to up-cast.
           printPP' aMP -- No need to up-cast.

           -- moveX must be inherited from the remote parent
	   putStrLn "    after moving MP..."
           (nUpCastTo aMP PP) # moveX $ 10
           printPP' aMP -- No need to up-cast.

           -- Nominal not equal structural subtyping
	   printSP aSP
	   -- printSP aPP -- Error! Nominal type different
