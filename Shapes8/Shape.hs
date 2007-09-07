{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

-- Shapes with existential types


module Shape where


-- Data of shapes

data ShapeData =
     ShapeData { xData :: Int
               , yData :: Int }



-- Concrete data with an interface

data ShapeC s = 
     ShapeC { getData  :: s
            , moveToI  :: Int -> Int -> s -> s
	    , rMoveToI :: Int -> Int -> s -> s
	    , drawI    :: s -> IO () }


-- Abstracted data with an interface

data ShapeA = forall s. ShapeA (ShapeC s)


-- Public interface of shapes

moveTo :: Int -> Int -> ShapeA -> ShapeA
moveTo x y (ShapeA s) = ShapeA (updData s (moveToI s x y))

rMoveTo :: Int -> Int -> ShapeA -> ShapeA
rMoveTo dx dy (ShapeA s) = ShapeA (updData s (rMoveToI s dx dy))

draw :: ShapeA -> IO ()
draw (ShapeA s) = drawI s (getData s)


-- Update data par of shape records

updData s f = s { getData = f (getData s) }


-- Reusable definitions of moving methods

moveTo' x y s = ShapeData x y 
rMoveTo' dx dy s = moveTo' x y s
  where
    x = xData s + dx
    y = yData s + dy
