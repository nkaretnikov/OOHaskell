-- Haskell98!

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

-- Emulating Existential.hs without the use of existentials.
-- Closure is provides abstraction, too.

module Existential1 where

import Shape
import Circle
import Rectangle


-- An `existential' envelope for shapes

data OpaqueShape = 
    OpaqueShape{sh_moveTo  :: Int -> Int -> OpaqueShape,
		sh_rMoveTo :: Int -> Int -> OpaqueShape,
		sh_draw    :: IO ()}


-- Opaque shapes are shapes

instance Shape OpaqueShape 
 where
  moveTo x y    s = sh_moveTo s x y
  rMoveTo dx dy s = sh_rMoveTo s dx dy
  draw          s = sh_draw s


-- The following two injections could be collected in a type class

inj_rectangle rect = 
    OpaqueShape{sh_moveTo  = \x y   -> inj_rectangle $ moveTo x y rect,
		sh_rMoveTo = \dx dy -> inj_rectangle $ rMoveTo dx dy rect,
		sh_draw    = draw rect}


inj_circle circ = 
    OpaqueShape{sh_moveTo  = \x y   -> inj_circle $ moveTo x y circ,
		sh_rMoveTo = \dx dy -> inj_circle $ rMoveTo dx dy circ,
		sh_draw    = draw circ}

-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ inj_rectangle (rectangle 10 20 5 6)
                        , inj_circle (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x)
               )
               scribble

         -- Handle rectangle-specific instance
         let r = rectangle 0 0 15 15
         let r' = r { widthData = 30 } 
         draw r' 
