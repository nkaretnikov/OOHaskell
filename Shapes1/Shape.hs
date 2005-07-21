
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where


-- The datatype for all forms of shapes

data Shape =
             Rectangle { getX      :: Int
                       , getY      :: Int 
                       , getWidth  :: Int 
                       , getHeight :: Int
                       }
           |
             Circle { getX      :: Int
                    , getY      :: Int 
                    , getRadius :: Int
                    }


-- Total setters

setX :: Int -> Shape -> Shape
setX i s = s { getX = i } 

setY :: Int -> Shape -> Shape
setY i s = s { getY = i } 


-- Partial setters

setWidth :: Int -> Shape -> Shape
setWidth i s = s { getWidth = i } 

setHeight :: Int -> Shape -> Shape
setHeight i s = s { getHeight = i } 

setRadius :: Int -> Shape -> Shape
setRadius i s = s { getRadius = i } 


-- Moving shapes

moveTo :: Int -> Int -> Shape -> Shape
moveTo x y = setY y . setX x 

rMoveTo :: Int -> Int -> Shape -> Shape
rMoveTo deltax deltay s = moveTo x y s
 where
  x = getX s + deltax
  y = getY s + deltay


-- A function for drawing shapes

draw :: Shape -> IO ()

draw s@(Rectangle _ _ _ _)
         =  putStrLn ("Drawing a Rectangle at:("
         ++ (show (getX s))
         ++ ","
         ++ (show (getY s))
         ++ "), width " ++ (show (getWidth s))
         ++ ", height " ++ (show (getHeight s)))

draw s@(Circle _ _ _)
         =  putStrLn ("Drawing a Circle at:("
         ++ (show (getX s))
         ++ ","
         ++ (show (getY s))
         ++ "), radius "
         ++ (show (getRadius s)))
