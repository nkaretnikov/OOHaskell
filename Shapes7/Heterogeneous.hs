
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Heterogeneous where

import Shape
import Circle
import Rectangle


class ScribbleLoop l 
 where
   scribbleLoop :: l -> IO ()

instance ScribbleLoop ()
  where
   scribbleLoop _ = return ()

instance (Shape h, ScribbleLoop t) => ScribbleLoop (h,t)
  where
   scribbleLoop (h,t)
     = do
          draw h
          draw (rMoveTo 100 100 h)
          scribbleLoop t


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble =  ( rectangle 10 20 5 6,
                                ( circle 15 25 8, 
                                ( ) ) )
         scribbleLoop scribble

         -- Handle rectangle-specific instance
         let r = rectangle 0 0 15 15
         let r' = r { widthData = 30 } 
         draw r' 
