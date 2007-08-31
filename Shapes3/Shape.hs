
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where

-- Recursive type for shapes

data Shape w =
     Shape { getX      :: Int
           , getY      :: Int
           , setX      :: Int -> Shape w
           , setY      :: Int -> Shape w
           , moveTo    :: Int -> Int -> Shape w
           , rMoveTo   :: Int -> Int -> Shape w
           , draw      :: IO ()
           , shapeTail :: w
           }


-- Constructor for shapes

shape x y d w
  = Shape { getX      = x
          , getY      = y
          , setX      = \x' -> shape x' y d w
          , setY      = \y' -> shape x y' d w
          , moveTo    = \x' y' -> shape x' y' d w
          , rMoveTo   = \deltax deltay -> shape (x+deltax) (y+deltay) d w
          , draw      = d x y
          , shapeTail = w
          }
