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

infixr 9 #
m # field = m .!. field

-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data MutableX s; mutableX (_::ST s t)  = proxy::Proxy (MutableX s)
data GetX s;     getX (_::ST s t)      = proxy::Proxy (GetX s)
data MoveD s;    moveD (_::ST s t)     = proxy::Proxy (MoveD s)
data OffsetX s;  offsetX (_::ST s t)   = proxy::Proxy (OffsetX s)


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
      let me = readSTRef x -- this is just the most weird way of getting 's'
      return $ 
	        (mutableX me) .=. x
	    .*. (getX me)     .=. readSTRef x
            .*. (offsetX me)  .=. method_offset x x_init
            .*. (moveD me)    .=. method_move x
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

testoST1 = 
    let 
      test :: ST s String
      test = do
	      printbuf <- newSTRef ""
	      let me = readSTRef printbuf
	      p <- class_pointST 1
	      (p # (getX me)) >>= (printST printbuf)
	      p # (moveD me) $ 2
	      (p # (getX me)) >>= (printST printbuf)
	      (p # (offsetX me)) >>= (printST printbuf)
	      readSTRef printbuf
    in runST test
