{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
-- We need overlapping instances SOLELY for the sake of Label4 below.
-- We could use (and have used) other ways of representing labels,
-- such as Label2. The latter requires no overlapping instances.
-- However, Label4 labels look better in types.

-- Link HList source directory to HList subdir.
-- Use gmake simple
-- which expands to ghci -i./HList  simple.hs

-- Simple objects, classes, interfaces (not involving open recursion,
-- i.e., the use of self)
-- In the following, we refer to the tutorial "Objects in Caml"
-- http://caml.inria.fr/ocaml/htmlman/manual005.html
-- Sec 3.1 Classes and objects


-- Using objects with mutable fields in the context of ST monad
-- Notable point: our labels and objects must depend on the
-- type 's' that represents a thread. This is natural:
-- once an object is instantiated, it can be used only in the same
-- thread that created that. In that sense, objects are a generalization
-- of a STRef.


module SimpleSTObj where


import CommonMain hiding (HDeleteMany, hDeleteMany, TypeCast,typeCast)
import GhcSyntax
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import Label4
import Data.Typeable -- needed for showing labels
import Data.STRef
import Data.IORef
import Control.Monad.ST

-- First, declare the labels.
-- We use proxies as of HList/Label4.hs
-- We explicitly carry the ST thread type 's' in the type of the label

data MutableX; mutableX (_::st s t)  = proxy::Proxy (s, MutableX)
data GetX;     getX (_::st s t)      = proxy::Proxy (s, GetX)
data MoveD;    moveD (_::st s t)     = proxy::Proxy (s, MoveD)
data OffsetX;  offsetX (_::st s t)   = proxy::Proxy (s, OffsetX)

-- Our records are wrapped up in STRecord that carries the 's'
-- type (as a phantom type)

newtype STRecord s r = STRecord r

-- We modify the field applicator '#' to extract 's' from the object
-- and pass it to the label constructor. This will guarantee that
-- the desirable label has the same thread type 's' as that of the object.
-- This convention also makes _using_ of ST objects look precisely the
-- same as _using_ of the IORef objects.

infixr 9 #
m@(STRecord r) # field = r .!. (field m)

blessST (_::st s a) (r::r)  = (STRecord r) :: STRecord s r

-- test that hLookupByHNat works even on lists that contain ST s t
-- with s being the quantified (eventually) variable
test1 = do 
	x <- newSTRef True
	let p = HCons x (HCons (readSTRef x) HNil)
	let v = hLookupByHNat (undefined::HZero) p
	let v1 = hLookupByHNat (undefined::(HSucc HZero)) p
	vv <- v1
	return vv
test11 = runST test1


-- Methods can be declared separately. In that case, they are all 
-- surely shared across all objects

method_move x d = modifySTRef x ((+) d)
method_offset x origin = do{v<-readSTRef x; return$ v - origin}

class_pointST x_init
  = do
      x <- newSTRef x_init
      return $ blessST x $
	        (mutableX x) .=. x
	    .*. (getX x)     .=. readSTRef x
            .*. (offsetX x)  .=. method_offset x x_init
            .*. (moveD x)    .=. method_move x
            .*. emptyRecord

printST buf val = 
    do
      v <- readSTRef buf
      writeSTRef buf (v ++ (show val) ++ "\n")

{-
testp3 = do
	  print "testp3"
	  p <- class_point 1
	  p # getX >>= print
	  p # moveD $ 2
	  p # getX >>= print
	  p # offsetX >>= print
	  print "OK"
-}

-- Note that the syntax remains the same as in the simple.st file
testoST1 = 
    let 
      test :: ST s String
      test = do
	      printbuf <- newSTRef ""
	      p <- class_pointST 1
	      p # getX >>= (printST printbuf)
	      p # moveD  $ 2
	      p # getX >>= (printST printbuf)
	      p # offsetX >>= (printST printbuf)
	      readSTRef printbuf
    in runST test

-- We now show an example of two objects, of reading something from
-- one object and putting into the other.
-- Both objects are within the same ST thread.

testoST2 = 
    let 
      test :: ST s String
      test = do
	      printbuf <- newSTRef ""
	      p1 <- class_pointST 1
	      p2 <- class_pointST 10
	      p1 # getX >>= (printST printbuf)
	      p2 # getX >>= (printST printbuf)
	      -- reading from one and putting into the other
	      p1 # getX >>= (p2 # moveD) 
	      p1 # getX >>= (printST printbuf)
	      p2 # getX >>= (printST printbuf)
	      readSTRef printbuf
    in runST test


main = do 
          testoST1
          testoST2
