{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fth #-}

{-

OOHaskell (C) 2004 -- 2007, Oleg Kiselyov, Ralf Laemmel

We illustrate that cloning cannot be easily provided.
The trouble is that resolved open recursion means that self can't be changed.

-}


module MultipleInheritance where

import OOHaskell
import OCamlTutorial hiding (main)
import Prelude hiding (print)


------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.14 Cloning objects
--

{- Ocaml Tutorial:

Objects can also be cloned, whether they are functional or
imperative. The library function Oo.copy makes a shallow copy of an
object. That is, it returns an object that is equal to the previous
one. The instance variables have been copied but their contents are
shared. Assigning a new value to an instance variable of the copy
(using a method call) will not affect instance variables of the
original, and conversely. A deeper assignment (for example if the
instance variable if a reference cell) will of course affect both the
original and the copy.

The type of Oo.copy is the following:

#Oo.copy;;
- : (< .. > as 'a) -> 'a = <fun>

The keyword as in that type binds the type variable 'a to the object
type < .. >. Therefore, Oo.copy takes an object with any methods
(represented by the ellipsis), and returns an object of the same
type. The type of Oo.copy is different from type < .. > -> < .. > as
each ellipsis represents a different set of methods. Ellipsis actually
behaves as a type variable.

#let p = new point 5;;
val p : point = <obj>
 
#let q = Oo.copy p;;
val q : point = <obj>
 
#q#move 7; (p#get_x, q#get_x);;
- : int * int = (5, 12)

-}

class OoCopy x
 where
  ooCopy :: x -> IO x

instance OoCopy (Record HNil)
 where
  ooCopy = return

instance ( IsTC1 v IORef b
         , OoCopy' b v
         , OoCopy (Record r)
         )
           => OoCopy (Record (HCons (LVPair l v) r))
 where
  ooCopy (Record (HCons h t)) =
    do
       let v = valueLVPair h
       v' <- ooCopy' (undefined::b) v
       let h' = newLVPair (undefined::l) v'
       (Record t') <- ooCopy (Record t)
       return $ Record (HCons h' t')


class OoCopy' b v
 where
  ooCopy' :: b -> v -> IO v

instance OoCopy' HFalse v
 where
  ooCopy' _ = return

instance OoCopy' HTrue (IORef v)
 where
  ooCopy' _ r =
    do
       v <- readIORef r
       newIORef v


myCloningOOP =
   do
      p <- mfix $ printable_point 5
      q <- ooCopy p
      q # moveX $ 7
      p # getX >>= putStrLn . show -- prints 12 (instead of 5)
      q # getX >>= putStrLn . show -- prints 12 fine
      readIORef (q # varX) >>= putStrLn . show


main = myCloningOOP
