
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module ConsEither where

import Shape
import Circle
import Rectangle
import Polymorph hiding (main)


-- Weirich's / Rathman's test case

main =
      do
           -- Handle the shapes polymorphically
           let scribble = cons (rectangle 10 20 5 6)
                        [circle 15 25 8]
           mapM_ ( \x -> 
                    do
                       draw x
                       draw (rMoveTo 100 100 x) )
                 scribble

           -- Handle rectangle-specific instance
           let r = rectangle 0 0 15 15
           let r' = r { widthData = 30 } 
           draw r' 

-- A union-constructing cons operation
cons :: h -> [t] -> [Either h t]
cons h t = Left h : map Right t 
-- cons h t@(_:_) = Left h : map Right t 
-- cons _ _ = error "Cannot cons with empty tail!"
