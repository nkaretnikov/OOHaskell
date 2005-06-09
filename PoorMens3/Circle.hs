{-# OPTIONS -fglasgow-exts #-}

module Circle where

import Shape

-- The delta of rectangles
data Circlish w =
     Circlish { getRadius :: Int 
              , circleExt :: w
              }

-- An extension of Shape
type Circle w = Shape (Circlish w)
circle x y r = shape x y $ Circlish { getRadius = r
                                    , circleExt = ()
                                    }

-- Setter
setRadius :: Int -> Circle w -> Circle w
setRadius i s = s { shapeExt = (shapeExt s) { getRadius = i } }

-- Implement abstract draw method
instance Draw (Circlish w)
 where
  draw s =  putStrLn ("Drawing a Circle at:("
         ++ (show (getX s))
         ++ ","
         ++ (show (getY s))
         ++ "), radius "
         ++ (show (getRadius (shapeExt s))))

