{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Existential where

import Shape hiding (draw,moveTo,rMoveTo)
import qualified Shape (draw,moveTo,rMoveTo)
import Circle
import Rectangle
import Control.Monad.Fix
import Data.IORef


data AbstractShape = forall x. AbstractShape (Shape x)

draw (AbstractShape s) = Shape.draw s
moveTo (AbstractShape s) = Shape.moveTo s
rMoveTo (AbstractShape s) = Shape.rMoveTo s


-- Weirich's / Rathman's test case

main =
      do

         -- Handle the shapes polymorphically
         s1 <- mfix $ rectangle 10 20 5 6
         s2 <- mfix $ circle 15 25 8
         let scribble = [ AbstractShape s1
                        , AbstractShape s2
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      rMoveTo x 100 100
                      draw x
               )
               scribble

         -- Handle rectangle-specific instance
         s3 <- mfix $ rectangle' 0 0 15 15
         setWidth s3 30
         Shape.draw s3
