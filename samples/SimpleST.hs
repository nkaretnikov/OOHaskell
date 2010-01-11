{-# LANGUAGE EmptyDataDecls #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This is a variation on SimpleIO.hs. (In fact, this is not a complete
reconstruction, but only a sketch.)  Using objects with mutable fields
in the context of ST monad Notable point: our labels and objects must
depend on the type 's' that represents a thread. This is natural: once
an object is instantiated, it can be used only in the same thread that
created that. In that sense, objects are a generalization of a STRef.

-}



module SimpleST where

import OOHaskell hiding (( # ))

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

blessST :: st s a -> r -> STRecord s r
blessST _ r  = STRecord r


-- First, declare the labels.
-- We use proxies as of HList/Label4.hs
-- We explicitly carry the ST thread type 's' in the type of the label

data MutableX; mutableX::st s t -> Proxy (s, MutableX); mutableX=undefined
data GetX;     getX::st s t ->Proxy (s, GetX);          getX=undefined
data MoveD;    moveD::st s t ->Proxy (s, MoveD);        moveD=undefined
data OffsetX;  offsetX::st s t ->Proxy (s, OffsetX);    offsetX=undefined


-- test that hLookupByHNat works even on lists that contain ST s t
-- with s being the quantified (eventually) variable
test1 :: ST s Bool
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
