module Rectangle(Rectangle,RectangleInstance(MakeRectangle), getWidth, getHeight, setWidth, setHeight)

   where
   import Shape

   -- declare method interfaces for rectangle subclass
   class Shape a => Rectangle a where
      getWidth :: a -> Int
      getHeight :: a -> Int
      setWidth :: a -> Int -> a
      setHeight :: a -> Int -> a

   -- define the methods for shape superclass
   instance Shape RectangleInstance where
      getX = x
      getY = y
      setX a newx = a {x = newx}
      setY a newy = a {y = newy}
      moveTo a newx newy = a {x = newx, y = newy}
      rMoveTo a deltax deltay = a {x = ((getX a) + deltax), y = ((getY a) + deltay)}
      draw a =
         putStrLn ("Drawing a Rectangle at:(" ++ (show (getX a)) ++ "," ++ (show (getY a)) ++
            "), width " ++ (show (getWidth a)) ++ ", height " ++ (show (getHeight a)))

   -- define the methods for rectangle subclass
   instance Rectangle RectangleInstance where
      getWidth = width
      getHeight = height
      setWidth a newwidth = a {width = newwidth}
      setHeight a newheight = a {height = newheight}

   -- declare the constructor for rectangle class
   data RectangleInstance = MakeRectangle {x, y, width, height :: Int}
      deriving(Eq, Show)
