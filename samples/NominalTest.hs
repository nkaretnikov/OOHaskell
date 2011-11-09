{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

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


-- Phantom types for nominal type distinctions

data PP  -- Printable points
data CP  -- Colored points
data MCP -- Massive Colored points
data SP  -- "Special" points


-- Declare subtyping relationships

instance Parents PP HNil           -- Printable points don't have parents
instance Parents CP  (PP :*: HNil) -- Colored points are printable points
instance Parents MCP (CP :*: HNil) -- Massive colored points are colored points
instance Parents SP  (PP :*: HNil) -- Special points are printable points


-- The familiar printable point but nominal this time

printable_point x_init s =
   do
      x <- newIORef x_init
      return 
        $  nominate (undefined::PP)
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
      return 
        $  nominate (undefined::CP)
        $  printi .=. ( do  putStr "so far - "; super # printi
                            putStr "color  - "; Prelude.print color )
       .<. getColor .=. color
       .*. anonymize super


-- Massive Colored points exercising overriding and extension

mcolored_point x_init color (mass::Float) self =
   do
      super <- colored_point x_init color self
      return 
        $  nominate (undefined::MCP)
        $  printi .=. ( do  putStr "so far - "; super # printi
                            putStr "mass  - "; Prelude.print mass )
       .<. getMass .=. mass
       .*. anonymize super


-- Special points that are structurally equal to PP

special_point x_init self = 
   do
      super <- printable_point x_init self
      return
        $ nominate (undefined::SP)
        $ anonymize super


-- These versions insist on certain nominal types.

printPP aPP = (aPP `hasNomination` (undefined::PP)) # print
printCP aCP = (aCP `hasNomination` (undefined::CP)) # print
printSP aSP = (aSP `hasNomination` (undefined::SP)) # print


-- These versions perform upcasts where necessary and possible.

printPP' o = printPP (nUpCast o)
printCP' o = printCP (nUpCast o)
printSP' o = printSP (nUpCast o)


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
           printPP (nUpCast aMP)
           printPP (nUpCast aSP)

           putStrLn "Nominal subtyping with implicit up-cast"
	   printPP' aPP
           printPP' aCP -- No need to up-cast.
           printPP' aMP -- No need to up-cast.
           printPP' aSP -- No need to up-cast.

           -- moveX must be inherited from the remote parent
	   putStrLn "    after moving MP..."
           (nUpCastTo aMP (undefined::PP)) # moveX $ 10
           printPP' aMP -- No need to up-cast.

           -- Nominal not equal structural subtyping
	   printSP aSP
	   -- printSP aPP -- Error! Nominal type different
