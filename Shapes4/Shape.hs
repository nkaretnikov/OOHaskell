
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where

import GHC.IOBase
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

shape x y concreteDraw tail self
  = do
       xRef  <- newIORef x
       yRef  <- newIORef y
       tail' <- tail
       returnIO Shape
                 { getX      = readIORef xRef
                 , getY      = readIORef yRef
                 , setX      = \x' -> writeIORef xRef x'
                 , setY      = \y' -> writeIORef yRef y'
                 , moveTo    = \x' y' -> do { setX self x'; setY self y' }
                 , rMoveTo   = \deltax deltay -> 
                                 do
                                    x <- getX self
                                    y <- getY self
                                    moveTo self (x+deltax) (y+deltay)
                 , draw      = concreteDraw self
                 , shapeTail = tail' self
                 }


-- An alternative constructor

shape' x y tail self
  = do
       xRef  <- newIORef x
       yRef  <- newIORef y
       tail' <- tail
       returnIO Shape
                 { getX      = readIORef xRef
                 , getY      = readIORef yRef
                 , setX      = \x' -> writeIORef xRef x'
                 , setY      = \y' -> writeIORef yRef y'
                 , moveTo    = \x' y' -> do { setX self x'; setY self y' }
                 , rMoveTo   = \deltax deltay -> 
                                 do
                                    x <- getX self
                                    y <- getY self
                                    moveTo self (x+deltax) (y+deltay)
                 , draw      = putStrLn "Nothing to dwaw"
                 , shapeTail = tail' self
                 }


-- OOish syntax

infixl 7 <<
a << m = a >> (m >>= (putStr . show))

newtype LS = LS String

ls :: Monad m => String -> m LS
ls = return . LS

instance Show LS
 where
  show (LS x) = x
