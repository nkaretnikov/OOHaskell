{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

OOHaskell (C) 2004, 2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

We show that OOHaskell allows for covariant argument types, while
maintaining type safety. We also refer to the module EiffelFaqLcon.hs,
where we also encoded a folklore example from the Eiffel FAQ.

-}

module CovariantArgs where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)
import DeepSubtyping hiding (test1,test2,main)


-- We continue the example on "deep subtyping"; cf. DeepSubtyping.hs
-- We extend the vector template by a move method for the origin.

data MoveO; moveO = proxy::Proxy MoveO

vector1 (p1::p) (p2::p) self =
   do super <- vector p1 p2 self
      returnIO
         $  moveO .=. (\p ->
                          do p1 <- self # getP1
	 		     xold <- p1 # getX
	                     xnew <- p  # getX
			     p1 # moveX $ (xnew-xold))
        .*. super


-- Always move with a zero point.

move_origin_to_0 varg = 
    do
    zero <- mfix (printable_point 0)
    varg # moveO $ zero


-- Demo vectors and colored vectors

test1 = do
           putStrLn "test1"
           p1  <- mfix (printable_point 1)
           p2  <- mfix (printable_point 5)
           cp1 <- mfix (colored_point (10::Int) "red")
           cp2 <- mfix (colored_point 25 "red")
           v1  <- mfix (vector1 p1 p2)
           cv1 <- mfix (vector1 cp1 cp2)
           v1 # print
           cv1 # print
           putStrLn "Moving the origin to 0"
           move_origin_to_0 v1
           move_origin_to_0 cv1
           v1 # print
           cv1 # print
           putStrLn "OK"


-- We create a vector template, with a co-variant method

data SetO; setO = proxy::Proxy SetO

vector2 (p1::p) (p2::p) self =
   do p1r <- newIORef p1; p2r <- newIORef p2
      returnIO $
           getP1    .=. readIORef p1r
       .*. getP2    .=. readIORef p2r
       .*. setO     .=. writeIORef p1r
       .*. print    .=. do self # getP1 >>= ( # print )
			   self # getP2 >>= ( # print )
       .*. emptyRecord


-- A polymorphic method for setting the origin to zero

set_origin_to_0 varg = 
    do
    zero <- mfix (printable_point 0)
    varg # setO $ zero


test2 = do
           putStrLn "test2"
           p1  <- mfix (printable_point 1)
           p2  <- mfix (printable_point 5)
           cp1 <- mfix (colored_point (10::Int) "red")
           cp2 <- mfix (colored_point 25 "red")
           v2  <- mfix (vector2 p1 p2)
           cv2 <- mfix (vector2 cp1 cp2)
           v2 # print
           cv2 # print
           putStrLn "Setting the origin to 0"
           set_origin_to_0 v2
           -- The following gives a type error!
           -- Unsafe use of co-variance
           -- set_origin_to_0 cv2
           v2  # print
           cv2 # print

           -- Although cv2 is not a subtype of v2, fully,
           -- we can still substitute cv2 for v2 when it is safe.
	   putStr "Length of v2: "
           norm v2 >>= Prelude.print
	   putStr "Length of colored cv2: "
           norm cv2 >>= Prelude.print

           -- The following is a type error: can't subtype
	   -- Let vectors = [deep'narrow v2, deep'narrow cv2]
	   --	      `asTypeOf` [v2]

           -- So, we need to cast away that offending setO method
           simplev <- mfix (vector p1 p2)
	   let vectors = [deep'narrow v2, deep'narrow cv2]
	   	      `asTypeOf` [simplev]

	   putStrLn "Vectors"
           mapM_ (\v -> do
	                  v # print
	                  putStr "Length is "; norm v >>= Prelude.print)
	         vectors
           putStrLn "OK"


main = do test1; test2
