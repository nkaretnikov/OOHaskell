
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle
import Control.Monad.Fix
import Data.IORef


-- Narrow shapes to a uniform base type

narrowToShape :: Shape w -> Shape ()
narrowToShape s = s { shapeTail = () } 


-- Weirich's / Rathman's test case

main = do
         -- Handle the shapes polymorphically
         s1 <- mfix $ rectangle 10 20 5 6
         s2 <- mfix $ circle 15 25 8
         let scribble = [ narrowToShape s1
                        , narrowToShape s2
                        ]
         mapM_ (\s -> do draw s
                         rMoveTo s 100 100
                         draw s)
               scribble

         -- Handle rectangle-specific instance
         s3 <- mfix $ rectangle' 0 0 15 15
         setWidth s3 30
         draw s3


-- Test overriding

main' = do l <- newIORef 0
	   c <- mfix $ circle' 15 25 8 l
	   draw c
	   t <- readIORef l
	   print t
