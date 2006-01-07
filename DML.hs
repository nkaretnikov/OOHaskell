{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module DML where

import OOHaskell
import Prelude hiding (print)
import qualified Prelude (print)


-- Labels

data Value deriving Typeable; value = proxy::Proxy Value
data Left  deriving Typeable; left  = proxy::Proxy Left
data Right deriving Typeable; right = proxy::Proxy Right
data Print deriving Typeable; print = proxy::Proxy Print


-- Idiom: properties

get = fst
p `set` x = (snd p) x
property ref = ( readIORef ref, writeIORef ref )


-- Int literals

lit (x::Int) self =
  do 
     xRef <- newIORef x
     returnIO $  value .=. property xRef
             .*. print .=. (do x <- get (self # value); putStr (show x))
             .*. emptyRecord


-- Add expressions

-- no type annotations are needed here
add l r self = 
  do
     lRef <- newIORef l
     rRef <- newIORef r
     returnIO $  left  .=. property lRef
             .*. right .=. property rRef
             .*. print .=. ( do 
                                l <- get (self # left)
                                l # print
                                putStr "+"
                                r <- get (self # right)
                                r # print
                           )
             .*. emptyRecord                                


-- A common base type for all expression forms

type Exp = Record ( Print :=: IO () :*: HNil )


-- Add expressions; using dynamics-based casts

addDyn l r self = 
  do
     lRef <- newIORef (dynUpCast l :: DynUpCast Exp)
     rRef <- newIORef (dynUpCast r :: DynUpCast Exp)
     returnIO $  left  .=. property lRef
             .*. right .=. property rRef
             .*. print .=. ( do 
                                (l::DynUpCast Exp) <- get (self # left)
                                l # print
                                putStr "+"
                                (r::DynUpCast Exp) <- get (self # right)
                                r # print
                                returnIO ()
                           )
             .*. emptyRecord                                


-- Add expressions; using forgetful narrow

addNarrow l r self = 
  do
     lRef <- newIORef ((narrow l) :: Exp)
     rRef <- newIORef ((narrow r) :: Exp)
     returnIO $  left  .=. property lRef
             .*. right .=. property rRef
             .*. print .=. ( do 
                                (l::Exp) <- get (self # left)
                                l # print
                                putStr "+"
                                (r::Exp) <- get (self # right)
                                r # print
                                returnIO ()
                           )
             .*. emptyRecord                                


-- Illustrations

test1 = do

           -- Construct and manipulate literal object
           lit1 <- mfix (lit 42)
           set (lit1 # value) 88
           int1 <- get (lit1 # value)
           Prelude.print int1
           lit1 # print; putStr "\n"

           -- Construct and fail to manipulate addition nodes
           lit2 <- mfix (lit 37)
           add1 <- mfix (add lit1 lit2)
           add2 <- mfix (add add1 lit1)
           add1 # print; putStr "\n"
           add2 # print; putStr "\n"
           set (add2 # right) lit2
           add2 # print; putStr "\n"
           --
           -- Cannot change type of children!
           --
           -- set (add2 # right) add1
           -- add2 # print; putStr "\n"

           Prelude.print "test1 done."


test2 = do

           -- Construct and manipulate literal object
           lit1 <- mfix (lit 42)
           lit2 <- mfix (lit 37)
           lit3 <- mfix (lit 88)
           lit1 # print; putStr "\n"
           lit2 # print; putStr "\n"
           lit3 # print; putStr "\n"
           add1 <- mfix (addDyn lit1 lit2)
           add2 <- mfix (addDyn add1 lit3)
           add1 # print; putStr "\n"
           add2 # print; putStr "\n"

           -- Apparant type change
           l <- get (add2 # left)    -- heavy tree
           r <- get (add2 # right)   -- light tree
           set (add2 # left) r       -- swap subtrees
           set (add2 # right) l      -- swap cont'd
           add2 # print; putStr "\n"

           -- Type change through setters
           set (add2 # left)  (dynUpCast lit1) -- Ooh!
           set (add2 # right) (dynUpCast lit1) -- Eeh!
           add2 # print; putStr "\n"

           Prelude.print "test2 done."


test3 = do

           -- Construct and manipulate literal object
           lit1 <- mfix (lit 42)
           lit2 <- mfix (lit 37)
           lit3 <- mfix (lit 88)
           lit1 # print; putStr "\n"
           lit2 # print; putStr "\n"
           lit3 # print; putStr "\n"
           add1 <- mfix (addNarrow lit1 lit2)
           add2 <- mfix (addNarrow add1 lit3)
           add1 # print; putStr "\n"
           add2 # print; putStr "\n"

           -- Apparant type change
           l <- get (add2 # left)    -- heavy tree
           r <- get (add2 # right)   -- light tree
           set (add2 # left) r       -- swap subtrees
           set (add2 # right) l      -- swap cont'd
           add2 # print; putStr "\n"

           -- Type change through setters
           set (add2 # left)  (narrow lit1) -- Ooh!
           set (add2 # right) (narrow lit1) -- Eeh!
           add2 # print; putStr "\n"

           Prelude.print "test3 done."


-- Add expressions

p `setN` x = (snd p) (narrow x)

-- make sure that when we put 'x' into an IORef, we can store into that IORef
-- both x and y (after narrowing)
-- This is better than C++: we do not need to declare the type of the field.
-- we need to specify which values we wish to store there.
-- BTW, any supertype of x and y can also be stored there, without additional
-- declaration. So, this function is needed only if x is not a supertype
-- of y
thisandalso x y = fst (lubNarrow x y)

test4 = do
           -- Construct and manipulate literal object
           lit1 <- mfix (lit 42)
           lit2 <- mfix (lit 37)
           lit3 <- mfix (lit 88)
           lit1 # print; putStr "\n"
           lit2 # print; putStr "\n"
           lit3 # print; putStr "\n"
	   let add1f self :: IO r1 = 
			     (add (thisandalso lit1 (undefined::r1))
			          (thisandalso lit2 (undefined::r1)) self)
	   (add1 :: r) <- mfix add1f
	   (add2 :: r) <- (mfix (add (narrow add1) (narrow lit3)))
	   add1 # print; putStr "\n"
	   add2 # print; putStr "\n"
           -- Apparant type change
           l <- get (add2 # left)    -- heavy tree
           r <- get (add2 # right)   -- light tree
           setN (add2 # left) r      -- swap subtrees
           setN (add2 # right) l      -- swap cont'd
           add2 # print; putStr "\n"

           -- Type change through setters
           setN (add2 # left)  lit1 -- Ooh!
           setN (add2 # right) lit1 -- Eeh!
           add2 # print; putStr "\n"
           Prelude.print "test4 done."

main = do
          test1
          test2
          test3
          test4

{-
tx () = do
     x <- newIORef (?b # print)
     y <- newIORef undefined
     lit1 <- mfix (lit 42)
     writeIORef y (narrow lit1)
     return (x,y)
     --Prelude.print "test4 done."
-}
