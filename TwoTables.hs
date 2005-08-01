{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-
 
OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

NOTE! UNDER DEVELOPMENT! 

A model of functional objects.
This code is not discussed in the OOHaskell paper.
It's included here as a starting point for future work.

-- Pure objects with open recursion
-- In the following, we refer to the tutorial "Objects in Caml"
-- http://caml.inria.fr/ocaml/htmlman/manual005.html
-- 3.12 Functional objects

An approach that uses separate tables for data and methods.

-}


module TwoTables where


import OOHaskell hiding ((.@.),( # ))


{- Ocaml Tutorial: 3.12 Functional objects

It is possible to write a version of class point without assignments
on the instance variables. The construct {< ... >} returns a copy of
``self'' (that is, the current object), possibly changing the value of
some instance variables.

class functional_point y =
   object
     val x = y
     method get_x = x
     method move d = {< x = x + d >}
   end;;
class functional_point :
  int ->
  object ('a) val x : int method get_x : int method move : int -> 'a end

let p = new functional_point 7;;
val p : functional_point = <obj>
 
p#get_x;;
- : int = 7

(p#move 3)#get_x;;
- : int = 10
 
p#get_x;;
- : int = 7

Note that the type abbreviation functional_point is recursive, which can be
seen in the class type of functional_point: the type of self is 'a and 'a
appears inside the type of the method move.

The above definition of functional_point is not equivalent to the following:

class bad_functional_point y =
   object
     val x = y
     method get_x = x
     method move d = new functional_point (x+d)
   end;;

While objects of either class will behave the same, objects of their subclasses
will be different. In a subclass of the latter, the method move will keep
returning an object of the parent class. On the contrary, in a subclass of the
former, the method move will return an object of the subclass.
-}

-- First, we declare the labels

data FieldX;  fieldX = proxy::Proxy FieldX
data GetX;    getX     = proxy::Proxy GetX
data MoveD;   moveD    = proxy::Proxy MoveD
data MoveD2;  moveD2   = proxy::Proxy MoveD2 -- move twice

data MTable; mTable  = proxy::Proxy MTable

{-
Then we may try the following (as a straight-forward extension
of mutable objects)

class_point y self
  = 	   fieldX .=. y
       .*. getX   .=. ( self # fieldX )
       .*. moveD  .=. (\d -> self .@. (fieldX,(d+( self # fieldX ))))
       .*. emptyRecord

testp1 = do
	  print "testp1"
	  -- Note that 'fix' plays the role of 'new' in the OCaml code...
	  let p = fix (class_point 7)
	  print (p # getX)
	  print ((p # moveD $ 3) # fieldX)
	  print (p # getX)
	  print "OK"


Alas, that doesn't work. The compiler reports "infinite type"
when attempting to perform `fix (class_point 7)'. The culprit is obvious:
the method moveD. It returns 'self' (the object with the updated
fieldX slot that is). But that is the problem: try

  let f self = (True,self)
  fix f

To solve the problem, we have to deeply understand the issue with the
quantification of moveD. What does method moveD do? It takes a number
and _any_ record with the slot fieldX and returns the record of the
same type but with the updated slot. Note _any_. So, the record type
of moveD must be locally universally quantified. The whole object
therefore has a higher-ranked type, and we need an explicit signature
sooner or later. In this file, we try to make explicit signatures as
localized as possible. 

Another solution: have moveD be a co-variant method, is discussed
elsewhere.

Actually, we've got another problem with the above solution:
even if moveD could be made to work as intended,
	((point # moveD) 3) # getX
would have printed the _old_ version of fieldX, although
	((point # moveD) 3) # fieldX
would print the modified version.

This is because getX, as defined above, operates on 'self' it received
at the creation of the object. But we need getX to use `self' after
it was modified by moveD. So, our objects will be a collection of
data fields and transformers self -> w or self -> self
Because we want self arguments above generalized, we have to play
tricks with higher-ranked types.

-}


-- Before we proceed, however, we need to define a different function
-- to update a slot `in-place'. The function `hUpdateAtLabel'
-- returns the record of a different type compared to the source record.
-- The following function takes a label, a value transformer, and
-- a record, and returns the record of the same type as the source
-- record, but with the modified value in the slot specified by the label.

hUpdateAtLabelInPlace (l::l) (f::v->v) (Record r) = Record r'
 where
  (ls,vs) = hUnzip r
  n       = hFind l ls
  t       = (\ (l,v) -> (l,f v)) :: ((l,v) -> (l,v))
  r'      = hUpdateAtHNatInPlace n t r

infixr 3 .@.
r .@. (l,f) =  hUpdateAtLabelInPlace l f r


-- An update `in-place' operation

class HNat n => HUpdateAtHNatInPlace n e l | n l -> e
 where
  hUpdateAtHNatInPlace :: n -> (e->e) -> l -> l

instance HUpdateAtHNatInPlace HZero e (HCons e l)
 where
  hUpdateAtHNatInPlace _ f (HCons e l) = HCons (f e) l

instance (HUpdateAtHNatInPlace n f l, HNat n)
      => HUpdateAtHNatInPlace (HSucc n) f (HCons e l)
 where
  hUpdateAtHNatInPlace n f (HCons e l)
   = HCons e (hUpdateAtHNatInPlace (hPred n) f l)



--An experiment with iso-recursive types

newtype Point self = Point (Record ((Proxy FieldX,Int) :*:
			      (Proxy GetX, self -> Int) :*:
			      (Proxy MoveD,Int -> self -> self) :*:
			      HNil))

data Fix f = Fix (f (Fix f))
--type PointObj v = Fix (Point v)

make_point x = Fix $ Point $ 
	        fieldX .=. x 
	    .*.	getX   .=. (\ (Fix (Point x)) -> x .!. fieldX)
	    .*. moveD  .=. (\d (Fix (Point x)) -> 
			    Fix $ Point $ x .@. (fieldX, (\v -> v+d)))
			    
	    .*. emptyRecord
--case (make_point 7) of (Fix (Point x)) -> x .!. fieldX

testp1 = do
	  print "testip1"
	  let p  = make_point 7
	  print $ case p of (Fix (Point x)) -> x .!. fieldX
	  print $ case p of (Fix (Point x)) -> (x .!. getX) p
	  let np = case p of (Fix (Point x)) -> (x .!. moveD) 3 p
	  print $ case np of (Fix (Point x)) -> (x .!. getX) np

{-
	  print (((p ## (class_point moveD)) 3) .!. fieldX)
	  -- Class is first class!
	  let c x = class_point x
	  print (((p ## (c moveD)) 3) ## (c getX))
	  print (p ## (c getX))
	  print (((p ## (c moveD2)) 3) ## (c getX))
-}
	  print "OK"



{-
-- Class of 1D points. The class is the dispatcher. It is the
-- common, shared part of all objects of its class

-- See http://pobox.com/~oleg/ftp/Scheme/pure-oo-system.scm

data FO message_table obj_data = FO message_table obj_data

data RD result obj_data = RD result obj_data
data RO result obj = RO result obj

res (RO result _) = result

class_point_message_table self obj_data
       =     getX   .=. RD (obj_data .!. fieldX) obj_data
         .*. moveD  .=. RD () (\d -> obj_data .@. (fieldX,(\v -> v + d)))
	 -- show off the recursive invocation, and open recursion
	 -- moveD2 applies moveD twice
--	 .*. moveD2 .=. (\d -> ((((recur self obj_data) # moveD) d) # moveD) d)
         .*. emptyRecord

make_class message_table label obj = (fix message_table) obj  .!. label

-- The 'label' argument is there just to cancel the monomorphism restriction
class_point label = make_class class_point_message_table label

recur mtable obj = FO (make_class mtable) obj

-- The 'variable' part of an object
obj_point x 
    =    fieldX .=. x
     .*. emptyRecord

{-
infixr 9 #
m # field = (((mtables () .!. (m .!. mTable)) m) .!. field)
-}

infixr 9 #
(FO mtable obj_data) # label = 
    let RD res obj_data' = (mtable label) obj_data
    in RO res (FO mtable obj_data')

infixr 9 ##
(RO _ obj) ## label =  obj # label

getField (FO mtable obj_data) label = obj_data .!. label

testp1 = do
	  print "testp1"
	  let p () = FO class_point (obj_point 7)
	  print $ (obj_point 7) .!. fieldX
--	  print $ (p ()) `getField` fieldX
	  print $ res $ (p ()) # getX
{-
	  print (((p ## (class_point moveD)) 3) .!. fieldX)
	  -- Class is first class!
	  let c x = class_point x
	  print (((p ## (c moveD)) 3) ## (c getX))
	  print (p ## (c getX))
	  print (((p ## (c moveD2)) 3) ## (c getX))
-}

	  print "OK"

-}
{-
-- Now we try to prove that our functional point is a good one
-- (see OCaml's tutorial quoted above)
-- That is, moveD takes effect even in subclasses

-- We turn a 1-D point into a 2-D point.
-- We add a new field fieldY, a new method getY
-- We override a method moveD
-- We add a method showP (which uses recursion)
data FieldY; fieldY = proxy::Proxy FieldY
data GetY;   getY   = proxy::Proxy GetY

-- declare a printable point label
data ShowP;  showP    = proxy::Proxy ShowP

class_point2D_message_table self obj
  =   	   getY   .=. (obj .!. fieldY)

	   -- recursive invocation
       .*. showP  .=. (unwords ["[", show (obj ## (recur self getX)), ",",
			             show (obj ## (recur self getY)), "]" ])

	   -- overriden method
       .*. moveD  .=. (\(d::Int) -> (obj .@. (fieldX,(\v -> v + d)))
				         .@. (fieldY,(\v -> v + d)))
       .*. ((class_point_message_table self obj) .-. moveD)

class_point2D label = make_class class_point2D_message_table label


-- The 'variable' part of an object
obj_point2D x y
    =    fieldY .=. y
     .*. obj_point x


testp2 = do
	  print "testp2"
	  let p = obj_point2D 5 7
	  let c x = class_point2D x
	  print (p ## (c getX))
	  print (p ## (c getY))
	  print (((p ## (c moveD)) 3) ## (c showP))
	  -- the following uses open recursion!!!
	  -- moveD2 is inherited from point1D but it uses
	  -- the overridden moveD
	  print (((p ## (c moveD2)) 3) ## (c showP))
	  print "OK"

-}


main = do testp1 -- ; testp2
