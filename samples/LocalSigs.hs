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

An approach that uses local signatures.

-}


module LocalSigs where

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

data FieldX; fieldX = proxy::Proxy FieldX
data GetX;   getX     = proxy::Proxy GetX
data MoveD;  moveD    = proxy::Proxy MoveD


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

class HNat n => HUpdateAtHNatInPlace n f l
 where
  hUpdateAtHNatInPlace :: n -> f -> l -> l

instance HUpdateAtHNatInPlace HZero (e->e) (HCons e l)
 where
  hUpdateAtHNatInPlace _ f (HCons e l) = HCons (f e) l

instance (HUpdateAtHNatInPlace n f l, HNat n)
      => HUpdateAtHNatInPlace (HSucc n) f (HCons e l)
 where
  hUpdateAtHNatInPlace n f (HCons e l)
   = HCons e (hUpdateAtHNatInPlace (hPred n) f l)


-- We also define an alias to help us with explicit signatures
-- and explicit quantifications

type UpdateableField field r v = forall x y n l'.
    (HZip x y r,
     HFind field x n,
     HUpdateAtHNatInPlace n ((field,v)->(field,v)) r) =>
    Record r -> Record r

-- Here we make a local signature and a higher-ranked type
data UF field a = UF (forall r. UpdateableField field r a)

make_uf:: field -> (a->a) -> UF field a
make_uf field f = UF (\self -> self .@. (field,f))

{-
type HasField field r v w = forall x y n. (HZip x y r, HFind field x n, 
					   HLookupByHNat n y v) =>
    Record r -> w
-}

data HF field a b = HF (forall r. HasField field r a => Record r -> b)
make_f:: field -> (a->b) -> HF field a b
make_f field tr = HF ( tr . (.!. field))

class_point (y::y) :: r
  = 	   fieldX .=. y
       .*. getX   .=. (make_f fieldX id)
       .*. moveD  .=. (\d -> make_uf fieldX ((\v->v+d)::(y->y)))
       .*. emptyRecord


infixr 9 #
m # field = case (m .!. field) of HF g -> g m


-- contra-variant return type...

contrav p l = \arg -> case (p .!. l) arg of UF g -> g p

testp1 = do
	  print "testp1"
	  -- Note that 'fix' plays the role of 'new' in the OCaml code...
	  let p = class_point 7
	  print (p # getX)
	  print (((p `contrav` moveD) 3) .!. fieldX)
	  print (((p `contrav` moveD) 3) # getX)
	  print (p # getX)
	  print "OK"

-- Now we try to prove that our functional point is a good one
-- (see OCaml's tutorial quoted above)
-- That is, moveD takes effect even in subclasses

-- declare a printable point label
data PrintP;  printP    = proxy::Proxy PrintP

-- and a printable point as a subclass of point
-- But that is difficult to do...


--make_af:: l -> (a1->b) -> HF l (HF l1 a a1) b
--make_af method tr = HF (\self-> case (self .!. method ) of HF g -> tr (g self))

class_printable_point y
  =         printP   .=. make_f getX id
       .*. (class_point y)


testp2 = do
	  print "testp2"
	  -- Note that 'fix' plays the role of 'new' in the OCaml code...
	  let p = class_printable_point 7
	  print (p # getX)
	  print (case (p # printP) of HF g -> show (g p))
	  print (((p `contrav` moveD) 3) .!. fieldX)
	  print (((p `contrav` moveD) 3) # getX)
	  print (p # getX)
	  print "OK"

main = do testp1; testp2
