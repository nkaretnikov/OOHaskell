{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Non-destructive upcast

module Upcast1 where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)


-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveX;     moveX     = proxy::Proxy MoveX
data Print;    print    = proxy::Proxy Print


printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveX     .=. (\d -> modifyIORef x (+d))
       .*. print    .=. ((s # getX ) >>= Prelude.print)
       .*. emptyRecord


-- We need another label.
data GetColor; getColor = proxy::Proxy GetColor

colored_point x_init (color::String) self =
   do
      super <- printable_point x_init self
      return
         $  getColor .=. (returnIO color)
	.*. (print .=. (
                       do  putStr "so far - "; super # print
                           putStr "color  - "; Prelude.print color )
             .<. super)


-- non-modifying upcast (from an _object_ (non-extensible!)) to another
-- non-extensible object
-- It is based on the tail-extensible record...
-- Note the duality with castEither!
data UC r ro = UC (Maybe r) (Maybe ro)

instance (HasField l r v, HasField l ro v) =>
         HasField l (UC r ro) v
  where
  hLookupByLabel l (UC (Just x) _) = x # l
  hLookupByLabel l (UC Nothing (Just x)) = x # l


test1 = do
      putStrLn "Just the printable point"
      p <- mfix (printable_point 7)
      p # moveX $ 2
      p # print

      putStrLn "Ditto via indirection"
      pr <- newIORef p
      readIORef pr >>= (\o -> o # print)

      c <- mfix (colored_point 9 "Red")
      -- the following is rightfully disallowed:
      -- writeIORef pr c
      putStrLn "Done"

-- Just to see the type...
test2 () = do
      putStrLn "UC printable point"
      p <- mfix (printable_point 7)
      pr <- newIORef (UC (Just p) Nothing)
      return pr

test3 = do
      putStrLn "UC printable point"
      p <- mfix (printable_point 7)
      pr <- newIORef (UC (Just p) Nothing)

      putStrLn "Printing the cast pt"
      -- Alas, if we remove "writeIORef pr (UC Nothing (Just c))"
      -- below, we fail to typecheck: because the constraints require
      -- that both components of UC have the label Print, but
      -- the second component is unknown and so the typechecker can't
      -- verify the constraint. IORef cannot store polymorphic values...
      -- (some typevariable with some constraint)
      readIORef pr >>= (\o -> o # print)

      c <- mfix (colored_point 9 "Red")
      writeIORef pr (UC Nothing (Just c))

      putStrLn "Printing the cast pt"
      readIORef pr >>= (\o -> o # print)

      putStrLn "Moving and modifying"
      readIORef pr >>= (\o -> o # moveX $ 10)
      readIORef pr >>= (\o -> o # print)

      putStrLn "Done"


