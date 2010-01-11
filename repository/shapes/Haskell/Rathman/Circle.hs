module Circle(Circle, CircleInstance(MakeCircle), getRadius, setRadius)

   where
   import Shape

   -- declare method interfaces for circle subclass
   class Shape a => Circle a where
      getRadius :: a -> Int
      setRadius :: a -> Int -> a

   -- define the methods for shape superclass
   instance Shape CircleInstance where
      getX = x
      getY = y
      setX a newx = a {x = newx}
      setY a newy = a {y = newy}
      moveTo a newx newy = a {x = newx, y = newy}
      rMoveTo a deltax deltay = a {x = ((getX a) + deltax), y = ((getY a) + deltay)}
      draw a =
         putStrLn ("Drawing a Circle at:(" ++ (show (getX a)) ++ "," ++ (show (getY a)) ++
            "), radius " ++ (show (getRadius a)))

   -- define the methods for circle subclass
   instance Circle CircleInstance where
      getRadius = radius
      setRadius a newradius = a {radius = newradius}

   -- declare the constructor for circle class
   data CircleInstance = MakeCircle {x, y, radius :: Int}
      deriving(Eq, Show)
