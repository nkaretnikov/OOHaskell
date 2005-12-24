{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fno-monomorphism-restriction #-}

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
baseProperty (ref::IORef x) = ( readIORef ref, \new -> writeIORef ref ( dynUpCast new :: x ) )


-- Int literals

lit (x::Int) self =
  do 
     xRef <- newIORef x
     returnIO $  value .=. property xRef
             .*. print .=. (do x <- get (self # value); putStr (show x))
             .*. emptyRecord


-- Add expressions

add (l::l) (r::r) self = 
  do
     lRef <- newIORef l
     rRef <- newIORef r
     returnIO $  left  .=. property lRef
             .*. right .=. property rRef
             .*. print .=. ( do 
                                (l::l) <- get (self # left)
                                l # print
                                putStr "+"
                                (r::r) <- get (self # right)
                                r # print
                           )
             .*. emptyRecord                                


-- A common base type for all expression forms

type Exp = Record ( Print :=: IO () :*: HNil )

{-
baseProperty (ref::IORef (DynUpCast Exp))
  = let (get,set) = property ref
     in (get,set . dynUpCast)
-}

-- Add expressions; using dynamics-based casts

addDyn l r self = 
  do
     lRef <- newIORef ((dynUpCast l) :: DynUpCast Exp)
     rRef <- newIORef ((dynUpCast r) :: DynUpCast Exp)
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
--           add2 # print; putStr "\n"
--           l <- get (add2 # left)
--           r <- get (add2 # right)
--           set (add2 # left) r
--           set (add2 # right) l
--           set (add2 # left) lit1
--           set (add2 # right) lit1
           add2 # print; putStr "\n"
--           set (add2 # right) lit2
--           add2 # print; putStr "\n"
           Prelude.print "test2 done."

main = do
          test1
          test2
