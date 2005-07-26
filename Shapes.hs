{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

-- The classic Shapes example
-- http://www.angelfire.com/tx4/cus/shapes/
-- We shall try to emulate the `classical' C++ implementation
-- http://www.angelfire.com/tx4/cus/shapes/cpp.html

-}


module Shapes where

import OOHaskell
import Record

infixr 9 #
m # field = (m .!. field) 


-- The class Shape

-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data GetX;     getX     = proxy::Proxy GetX
data GetY;     getY     = proxy::Proxy GetY
data SetX;     setX     = proxy::Proxy SetX
data SetY;     setY     = proxy::Proxy SetY
data MoveTo;   moveTo   = proxy::Proxy MoveTo
data RMoveTo;  rMoveTo  = proxy::Proxy RMoveTo
data Draw;     draw     = proxy::Proxy Draw

-- Note that unlike C++ version, our class is polymorphic in x and y.
-- Those fields can be any Num.
-- The fields are private, just as they are in C++ code

-- This is an optional part in case we want to fix types of virtuals.
-- We need this order unless we dare to see an overlapping pattern warning.

shape x_init y_init self 
  | const False ( narrow self ::
                    Record (  Draw :=: IO ()
                          :*: HNil ) )
  = undefined

-- This is the actual definition

shape x_init y_init self
  = do
      x <- newIORef x_init
      y <- newIORef y_init
      returnIO $
           getX     .=. readIORef x
       .*. getY     .=. readIORef y
       .*. setX     .=. (\newx -> writeIORef x newx)
       .*. setY     .=. (\newy -> writeIORef y newy)
       .*. moveTo   .=. (\newx newy -> do
                                        (self # setX) newx
                                        (self # setY) newy 
                        )
       .*. rMoveTo  .=. (\deltax deltay ->
                 do
                    x  <- self # getX
                    y  <- self # getY
                    (self # moveTo) (x + deltax) (y + deltay)
                        )
       .*. emptyRecord


-- We make the instantiation test.
-- This one should throw *if* the draw method is required.
-- Unfortunately, it does not throw when polymorphism is still present.
-- GHC 6.4 is very lazy when it comes to instance resolution.1

-- complete_shape  
--  = concrete $ shape (1::Int) (2::Int)

{-

-- A sort of type annotation
-- In general, we need explicit eq because of order of structural types

complete_shape = shape
 where
  _ = (mfix (shape (1::Int) (2::Int) . narrow)) :: IO (Shape Int)

-}

{-
    Couldn't match `(:*:) (Proxy Draw, IO ()) HNil' against `HNil'
      Expected type: Shape Int -> IO (Shape Int)
      Inferred type: Record ((:*:) (Proxy GetX, IO Int)
                                   ((:*:) (Proxy GetY, IO Int)
                                          ((:*:) (Proxy SetX, Int -> IO ())
                                                 ...
-}

-- Rectange: inherits from Shape
-- Again, it is polymorphic in the types of its fields

data GetWidth;    getWidth     = proxy::Proxy GetWidth
data GetHeight;   getHeight    = proxy::Proxy GetHeight
data SetWidth;    setWidth     = proxy::Proxy SetWidth
data SetHeight;   setHeight    = proxy::Proxy SetHeight

infixl 7 <<
a << m = a >> (m >>= (putStr . show))

newtype LS = LS String
ls = return . LS
instance Show LS where show (LS x) = x

rectangle x y width height self
  = do
      super <- shape x y self
      w <- newIORef width
      h <- newIORef height
      returnIO $
           getWidth  .=. readIORef w
       .*. getHeight .=. readIORef h
       .*. setWidth  .=. (\neww -> writeIORef w neww)
       .*. setHeight .=. (\newh -> writeIORef h newh)
       .*. draw      .=. 
           do
              putStr  "Drawing a Rectangle at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), width " << self # getWidth <<
                      ls ", height " << self # getHeight <<
                      ls "\n"
       .*. super

-- Square: inherits from Shape
-- Again, it is polymorphic in the types of its fields
-- Like Rectange, in has getWidth and setWidth. But it doesn't
-- have getHeight and setHeight methods. This is a bit contrived,
-- but important for the point we make in Encoding 5 below.

square x y width self
  = do
      super <- shape x y self
      w <- newIORef width
      returnIO $
           getWidth  .=. readIORef w
       .*. setWidth  .=. writeIORef w
       .*. draw      .=. 
           do
              putStr  "Drawing a Square at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), side " << self # getWidth <<
                      ls "\n"
       .*. super


-- Circle: inherits from Shape
-- Again, it is polymorphic in the types of its fields

data GetRadius;    getRadius     = proxy::Proxy GetRadius
data SetRadius;    setRadius     = proxy::Proxy SetRadius

circle x y radius self
  = do
      super <- shape x y self
      r <- newIORef radius
      returnIO $
           getRadius  .=. readIORef r
       .*. setRadius  .=. (\newr -> writeIORef r newr)
       .*. draw       .=. 
           do
              putStr  "Drawing a Circle at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), radius " << self # getRadius <<
                      ls "\n"
       .*. super


-- This interface is used in Encoding 1 below.  When we want to make a
-- list of abstract Shapes, then we cast objects to this interface.

type Shape a = Record (  GetX    :=: IO a
                     :*: GetY    :=: IO a
                     :*: SetX    :=: (a -> IO ())
                     :*: SetY    :=: (a -> IO ())
                     :*: MoveTo  :=: (a -> a -> IO ())
                     :*: RMoveTo :=: (a -> a -> IO ())
                     :*: Draw    :=: IO ()
                     :*: HNil )


-- In fact this interface would be sufficent
type Shape' a
 = Record (  (Proxy RMoveTo , a -> a -> IO ())
         :*: (Proxy Draw    , IO ())
         :*: HNil )


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


-- Encoding 1
-- We build a homogeneous list of shapes.
-- We use coercion to an interface to harmonise the different shapes.

myShapesOOP =
  do
       -- set up array of shapes
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble :: [Shape Int]
           scribble = [narrow s1, narrow s2]
       
       -- iterate through the array
       -- and handle shapes polymorphically
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- Encoding 2
-- We first build a heterogeneous list of shapes.
-- Then we map over this list to narrow all shapes to the LUB type.
-- Thereby we obtain a normal homogeneous list.
-- BTW, hLubNarrow could just as well be a multi-variate function

yaShapesOOP =
  do
       -- set up array of shapes
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble = hLubNarrow (HCons s1 (HCons s2 HNil))
       
       -- iterate through the array
       -- and handle shapes polymorphically
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


-- Encoding 3
-- We build a heterogeneous list of shapes.
-- We use a heterogeneous map over the list.
-- The polymorphic function of the map is an Apply instance.
-- We place the label constraints in this instance.

testHList =
  do
       -- set up list of shapes.
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble = s1 `HCons` (s2 `HCons` HNil)
       
       -- iterate through the list
       -- and handle shapes polymorphically
       hMapM_ (undefined::ScribbleBody) scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
       arec # draw


-- A type code for the polymorphic function on shapes
data ScribbleBody -- a type code only!

-- The polymorphic function on shapes
instance ( HasField (Proxy Draw) r (IO ())
         , HasField (Proxy RMoveTo) r (Int -> Int -> IO ())
         )
      => Apply ScribbleBody r (IO ())
  where
    apply _ x = do
                   x # draw
                   (x # rMoveTo) 100 100
                   x # draw


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


-- Encoding 4
-- This time we combine the use of existential quantification
-- with OOHaskell objects. That is we have quantify the methods
-- that are going to be invoked for the shape objects.
-- This quantification requires constraints which are not first-class.

testExist =
  do
       -- set up list of shapes.
       s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
       s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
       let scribble = [ HideShape s1
                      , HideShape s2 ]
       
       -- iterate through the list
       -- and handle shapes polymorphically
       mapM_ ( \(HideShape shape) -> do
                  shape # draw
                  (shape # rMoveTo) 100 100
                  shape # draw )
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) (15::Int) (15::Int))
       arec # setWidth $ 30
       arec # draw


-- The well-quantified existential wrapper
data OpaqueShape =
 forall x. ( HasField (Proxy Draw) x (IO ())
           , HasField (Proxy RMoveTo) x (Int -> Int -> IO ())
           ) => HideShape x

{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- Encoding 5
-- We build a homogeneous list of shapes.
-- We use coercion to an interface to harmonise the different shapes.
-- This is similar to encoding 2, but use a union-intersection type.
-- From it, we can downcast -- safely (we can project only what we have
-- injected)

stynamicDowncast =
  do
       -- set up array of shapes
       -- Need full instantiation for the downcasts to work...
       -- Actually, I just need a better comparsion function, but I'm
       -- in a hurry
       s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
       s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
       s3 <- mfix (square (35::Int) (45::Int) (8::Int))
       let scribble = union'inter (HCons s1 (HCons s2 (HCons s3 HNil)))
       
       -- iterate through the array
       -- and handle shapes polymorphically
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw

       -- iterate throw the array and downcasts
       mapM_ (\shape -> maybe (putStrLn "None")
	                      (\circ -> do circ # setRadius $ 10;
			                   circ # draw)
	                      ((downcast shape) `asTypeOf` (Just s2)))
             scribble


newtype UnionIntersection u = UnionIntersection u
unUI (UnionIntersection x) = x

class UI l r | l -> r where
    union'inter :: l -> [UnionIntersection r]

-- Actually, I should take care of duplicates and avoid
-- excessive injections. But it is already almost 2am...

instance UI (HCons obj HNil) (Either obj obj) where 
    union'inter (HCons obj HNil) = [UnionIntersection (Left obj)]

instance UI (HCons o2 t) r => 
    UI (HCons o1 (HCons o2 t)) (Either o1 r) where
    union'inter (HCons o t) = let ut = union'inter t
			      in UnionIntersection (Left o) : 
				 map (UnionIntersection . Right . unUI) ut

-- This essentially computes the intersection

instance (HasField l a v, HasField l b v) 
    => HasField l (Either a b) v 
 where
  hLookupByLabel l (Left a) =  hLookupByLabel l a
  hLookupByLabel l (Right b) =  hLookupByLabel l b

instance (HasField l u v)
    => HasField l (UnionIntersection u) v
 where
  hLookupByLabel l (UnionIntersection a) =  hLookupByLabel l a


class DownCast f t where
    downcast :: UnionIntersection f -> Maybe t

instance (TypeEq a t bf, Downcast' bf (Either a b) t) 
    => DownCast (Either a b) t where
    downcast (UnionIntersection u) = downcast' (undefined::bf) u

class Downcast' bf f t where
    downcast' :: bf -> f -> Maybe t

instance Downcast' HTrue (Either a b) a where
    downcast' _ (Left x) = Just x
    downcast' _ _ = Nothing

instance DownCast (Either c d) t
    => Downcast' HFalse (Either a (Either c d)) t where
    downcast' _ (Left x) = Nothing
    downcast' _ (Right x) = downcast (UnionIntersection x)



{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


main = do 
          putStrLn "testCoerce"; myShapesOOP
          putStrLn "testLub"; yaShapesOOP
          putStrLn "testHList";  testHList
          putStrLn "testExist";  testExist

-- :t mfix $ rectangle (1::Int) (2::Int) (3::Int) (4::Int)
