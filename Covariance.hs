{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

UNDER DEVELOPMENT

-- Playing with covariance
-- http://www.faqs.org/faqs/eiffel-faq/
-- Question 
-- LCON: Please explain and discuss covariance vs. contravariance.

-}


module Covariance where

import OOHaskell

infixr 9 #
m # field = (m .!. field) 


{-
Here's another example where a real-world situation suggests a
covariant solution. Herbivores eat plants. Cows are herbivores. Grass
is a plant. Cows eat grass but not other plants.

   class HERBIVORE                               class PLANT
   feature
      eat(food: PLANT) is ...
      diet: LIST[PLANT]

   class COW                                     class GRASS
   inherit                                       inherit
      HERBIVORE                                     PLANT
         redefine eat
      end
   feature eat(food: GRASS) is ...


-}

-- class and field labels
data CLPlant;   cl_plant   = proxy::Proxy CLPlant
data CLGrass;   cl_grass   = proxy::Proxy CLGrass
data Print;    printIt  = proxy::Proxy Print
data Eat;      eat  = proxy::Proxy Eat
data Diet;     diet  = proxy::Proxy Diet

{-
-- Note, the interface is polymorphic in the type of the value, a
type PPInterface a
 = Record (  (Proxy GetX    , IO a)
         :*: (Proxy MoveTo  , a -> IO ())
         :*: (Proxy Print    , IO ())
         :*: HNil )
-}

class_plant self 
  = do
      returnIO $
           cl_plant  .=. "Plant"
       .*. printIt   .=. print (self # cl_plant )
       .*. emptyRecord

class_grass self 
  = do
      plant <- class_plant self
      let printSuper = plant .!. printIt
      returnIO $
           cl_grass  .=. "Grass"
       .*. printIt   .=. do { print (self # cl_grass ); printSuper }
       .*. (plant .-. printIt)

type IPlant 
 = Record (  (Proxy CLPlant , String)
         :*: (Proxy Print    , IO ())
         :*: HNil )

type IPHerbFixed
 = Record (  (Proxy Eat      , IPlant -> IO ())
         :*: (Proxy Print    , IO ())
         :*: HNil )

assert_class cl label = const (return ()) (cl # label)

class_herbivore self 
  = do
      returnIO $
           printIt   .=. print "HERBIVORE"
       .*. eat       .=. herb_eats self
       .*. emptyRecord

--herb_eats self (food :: IPlant) = 
herb_eats self food = 
    do
    self # printIt
    print "Eats"
    food # printIt

test1 = do
	print "test1"
	plant <- mfix class_plant
	grass <- mfix class_grass
	herb  <- mfix class_herbivore
	herb # eat $ plant
	-- Alas, the explicit narrow seems to be needed here
	-- monomorphic restriction on herb...
	herb # eat $ narrow grass
	print "OK"

class_cow self 
  = do
      herb <- class_herbivore self
      let eatSuper = herb .!. eat
      returnIO $
           printIt   .=. print "Cow"
       .*. eat       .=. (\food -> do { assert_class food cl_grass;
					eatSuper food})
       .*. ((herb .-. printIt) .-. eat)

test2 = do
	print "test2"
	plant <- mfix class_plant
	grass <- mfix class_grass
	herb  <- mfix class_herbivore
	cow   <- mfix class_cow
	herb # eat $ plant
	-- cow  # eat $ plant -- that would be an error
	cow  # eat $ grass
	-- Alas, the explicit narrow seems to be needed here
	-- monomorphic restriction on herb...
	herb # eat $ narrow grass
	print "OK"

test3 = do
	print "test3"
	plant <- mfix class_plant
	grass <- mfix class_grass
	herb  <- mfix class_herbivore
	cow   <- mfix class_cow
	herb # eat $ plant
	-- cow  # eat $ plant -- that would be an error
	cow  # eat $ grass
	-- Alas, the explicit narrow seems to be needed here
	-- monomorphic restriction on herb...
	herb # eat $ narrow grass
	-- let herbl = [herb]
	-- let herbl1 = cow : herbl -- can't do that: eat method prevents
	-- let herbl1 = narrow cow : herbl -- that too: eat method prevents
	let herbl = [herb .-. eat]
	let herbl1 = narrow cow : herbl -- now that works
	mapM_ ( # printIt ) herbl1
	print "OK"

{- 
 Regarding the last block of commented lines above.
 The Eiffel FAQ says:

 "The compiler must stop us from putting a COW object into a HERBIVORE
attribute and trying to feed it a PLANT, but we shouldn't be trying to
do this anyway."

And indeed, GHC does stop us.
-}


main = do test1; test2; test3
