{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
 
module HMap where

import Data.IORef

-- easier to write then to do cvs update...
data HNil = HNil deriving Show
data HCons a b = HCons a b deriving Show

class HList l
instance HList HNil
instance HList l => HList (HCons a l)

-- Lists of pairs

class (HList l, HList f, HList s) => 
      HListOfPairs l f s | l -> f, l -> s, f s -> l where 
  hzip:: f -> s -> l
  hunzip1:: l -> f
  hunzip2:: l -> s

instance HListOfPairs HNil HNil HNil where
    hzip _ _ = HNil
    hunzip1 _ = HNil
    hunzip2 _ = HNil

instance HListOfPairs l f s => 
         HListOfPairs (HCons (a,b) l) (HCons a f) (HCons b s) where
    hzip (HCons a f) (HCons b s) = HCons (a,b) (hzip f s)
    hunzip1 (HCons (a,_) l) = HCons a (hunzip1 l)
    hunzip2 (HCons (_,b) l) = HCons b (hunzip2 l)
	 
-- BTW, it is certain that if HListOfPairs l f s holds, then the lengths
-- of all three lists are the same.
l1 = HCons (Just$ Just$ Just ()) $ HCons (Just$ Just ()) $
     HCons (Just ()) HNil
     
l2 = HCons True $ HCons 'a' $ HCons "ab" HNil

test1 = let l = hzip l1 l2 in (l, hunzip1 l, hunzip2 l)

	    
-- class HListOfPairs m f s => HMap m
class HMap m
instance HListOfPairs m f s => HMap m

-- No HSet constraint. I'm too lazy, due to Haskell
class HMapPrj m a b m1 | m a -> b m1 where
    prj:: m -> a -> (b,m1)

instance HMapPrj m HNil HNil m where
    prj m _ = (HNil,m)
    
instance (HMapAdd m1' a b m, HMapPrj m1' f s m1) =>
         HMapPrj m (HCons a f) (HCons b s) m1 where
    prj m (HCons a f) = let (b,m1') = prj1 m a
                            (s,m1)  = prj m1' f
			in (HCons b s, m1)


class HMapAdd m a b m1 | m1 a -> b m where
    prj1:: m1 -> a -> (b,m)
    
instance HMapAdd HNil a b (HCons (a,b) HNil) where
    prj1 (HCons (a,b) _) _ = (b,HNil)
    
instance HMapAdd m a b (HCons (a,b) m) where
    prj1 (HCons (a,b) m) _ = (b,m)
    
instance HMapAdd' m a b m1 => HMapAdd m a b m1 where
    prj1 = prj1'
    
class HMapAdd' m a b m1 | m1 a -> b m where
    prj1':: m1 -> a -> (b,m)

instance HMapAdd m a b m1 => 
         HMapAdd' (HCons (x,y) m) a b (HCons (x,y) m1) where
    prj1' (HCons x m1) a = let (b,m') = prj1 m1 a in (b, HCons x m')
    
-- inj:: (HMap m) => m -> (a,b) -> HCons (a,b) m
-- inj m x = HCons x m


hmap1 = hzip l1 l2 

--hmap2 = hProjectAway hmap1 (HCons x1 HNil)
hmap2 = snd $ prj hmap1 (HCons (Just$ Just ()) HNil)
--hmap3 = hProject hmap1 (HCons x0 (HCons x1 HNil))
hmap3 = fst $ prj hmap1 (HCons (Just ()) $ HCons (Just$ Just ()) HNil)

------------------------------------------------------------------------
-- Simulating open products (records as in OCaml)
-- In OCaml:
--let foo f = f#fld1;;
--val foo : < fld1 : 'a; .. > -> 'a = <fun>

infixr 9 #

m # field = fst$ prj1 m field

data Field1 = Field1
foo f = f # Field1
-- foo :: forall b m1 m. (HMapAdd m Field1 b m1) => m1 -> b

-- In the following, we refer to the tutorial "Objects in Caml"
-- http://caml.inria.fr/ocaml/htmlman/manual005.html

-- #class point x_init =
--    object 
--      val mutable x = x_init
--      method get_x = x
--      method get_offset = x - x_init
--      method move d = x <- x + d
--    end;;

data M_get_x = M_get_x
data M_get_offset = M_get_offset
data M_move = M_move

class_point x_init
  = do
      x <- newIORef x_init
      return $
	      (HCons (M_get_x,readIORef x)
	      (HCons (M_get_offset,do{v<-readIORef x; return$ v - x_init})
	      (HCons (M_move,\d -> do{v<-readIORef x; writeIORef x (d + v)})
	      HNil)))
	      
testo1 = do
           p <- class_point 1
	   p # M_get_x >>= print
	   -- (p # M_move) 2 exposes some bug in GHC...
	   let f = p # M_move
	   f 2
	   p # M_get_x >>= print
	   p # M_get_offset >>= print
	   
-- inheritance and overriding

-- define point2D with an additional coordinate y, new method get_y
-- and overridden method move
-- we also remove the method M_get_offset
-- (See 'hidden by signature matching method' in OCaml Object
-- tutorial)
-- So, M_get_offset becomes a private method...


data M_get_y = M_get_y

class_point2D x_init y_init
 = do
     y <- newIORef y_init
     xpoint <- class_point x_init
     return $
             (HCons (M_get_y,readIORef y)
	     (HCons (M_move, \d-> do
	                           vy <- readIORef y
				   writeIORef y (d+vy)
				   let f = xpoint # M_move
				   f d)
	      (snd$ prj xpoint (HCons M_move (HCons M_get_offset HNil)))))
	      
testo2 = do
          p <- class_point2D 1 10
	  p # M_get_x >>= print
	  p # M_get_y >>= print
	  let f = p # M_move
	  f 5
	  p # M_get_x >>= print
	  p # M_get_y >>= print
	   

-- interfaces
-- From OCaml Tutorial
-- Class interfaces are inferred from class definitions. They may also be
-- defined directly and used to restrict the type of a class. Like class
-- declarations, they also define a new type abbreviation.

type I_get_x a = HCons (M_get_x,IO a) HNil

-- a function that takes anything of the interface I_get_x
get_x:: (Show a, HMapAdd m M_get_x  (IO a) m1) => m1 -> IO ()
get_x m = m # M_get_x >>= print

-- both class_point and class_point2D object satisfy the interface for get_x
testo3 = do
          p1 <- class_point 42
	  p2 <- class_point2D 84 126
	  get_x p1
	  get_x p2
	  


-- we can also do polymorphic methods (forall in the method):
-- cf. Section 3.10 of the OCaml tutorial

-- using prj, we can trivially do coercions: if our class_point2D had
-- not hid M_get_offset, we could have coerced class_point2D into
-- class_point just by projecting (using prj) the former to the latter
-- We can also do mutually recursive objects and many other things of 
-- OCaml without doing violence to the type system (as OCaml obviously
-- does).

