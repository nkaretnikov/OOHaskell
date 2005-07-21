
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ Rectangle 10 20 5 6
                        , Circle 15 25 8
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x))
               scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 (Rectangle 0 0 15 15)
