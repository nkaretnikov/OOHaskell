
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where

import Data.IORef


-- Recursive type for shapes

data Shape w =
     Shape { getX      :: IO Int
           , getY      :: IO Int
           , setX      :: Int -> IO ()
           , setY      :: Int -> IO ()
           , moveTo    :: Int -> Int -> IO ()
           , rMoveTo   :: Int -> Int -> IO ()
           , draw      :: IO ()
           , shapeTail :: w
           }


-- Constructor for shapes

shape x y d t
  = do
       xRef <- newIORef x
       yRef <- newIORef y
       t'   <- t
       let s = Shape
                { getX      = readIORef xRef
                , getY      = readIORef yRef
                , setX      = \x' -> writeIORef xRef x'
                , setY      = \y' -> writeIORef yRef y'
                , moveTo    = \x' y' -> do { setX s x'; setY s y' }
                , rMoveTo   = \deltax deltay -> 
                                do
                                   x <- getX s
                                   y <- getY s
                                   moveTo s (x+deltax) (y+deltay)
                , draw      = d s
                , shapeTail = t' s
                }
       return s


-- OOish syntax

infixl 7 <<
a << m = a >> (m >>= (putStr . show))

newtype LS = LS String

ls :: Monad m => String -> m LS
ls = return . LS

instance Show LS
 where
  show (LS x) = x
