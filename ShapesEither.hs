{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example. We encode subtyping through
open-ended normal disjoint unions. When we attempt record look-up, we
simply require that both summands admit the look-up since we could
find that any summand is actually inhabited at the value level. Two
strengths of this approach are these: it readily allows for
down-casts, and no nominal up-casts are required from the
programmer. Down-casts are completely type-safe; we can only attempt
to cast to types that are part of the union. In order to make all this
work, we need to define a few new type-level functions. These new
functions may eventually be moved to the HList or the OOHaskell
libraries.

We note that consEither, nilEither are generalizations of ((:),[]).
In fact, the former reduce to the latter in the case of homogeneous
lists.
Our building of unions is efficient: if the new element is already
in the union, we inject it into the existing uniuon rather than
extend the union.
See the tests te5-te7 below.

-}

module ShapesUnion where

import OOHaskell
import Shapes
import TypeCastGeneric2


-- The polymorphic scribble loop.

main =
  do
       --
       -- Set up array of shapes.
       -- We need full instantiation for the downcasts to work.
       -- ... or a better comparsion function. (Omitted.)
       --
       s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
       s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
       s3 <- mfix (square (35::Int) (45::Int) (8::Int))

       -- We could have used a vararg function.
       let scribble =  consEither s2
                      (consEither s1
                      (consEither s2
                      (consEither s3
                       nilEither)))

       -- Iterate through the array
       -- and handle shapes polymorphically.
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw

       -- iterate through the array and downcast to cirlce
       mapM_ (\shape -> maybe (putStrLn "Not a circle.")
	                      (\circ -> do circ # setRadius $ 10;
			                   circ # draw)
	                      ((downCast shape) `asTypeOf` (Just s2)))
             scribble


-- List constructors that union as well

data NilEither
nilEither = undefined :: NilEither

class ConsEither h t l | h t -> l
 where
  consEither :: h -> t -> l

instance ConsEither e  NilEither [e]
 where
  consEither h _ = [h]

instance (TypeEqInc eu e f, ConsEither' f e eu l)
    => ConsEither e [eu] l where
  consEither h t = consEither' (tsearch (undefined::eu) (undefined::e)) h t


class ConsEither' f e eu l | f e eu -> l where
    consEither' :: f -> e -> [eu] -> l

instance ConsEither' TNone e eu [Either e eu] where
    consEither' _ h t = Left h : map Right t

instance ConsEither' TSame e e [e] where
    consEither' _ h t = h : t

instance ConsEither' (TContains e eu) e eu [eu] where
    consEither' (TContains inj _) h t = inj h : t


-- Compare the type t with the type tu and return:
--  TSame -- if t = tu
--  TContains t tu -- if tu is the type Either tul tur and 
--                      either tul or tur is or includes t
--  TNone -- otherwise

data TSame				-- outcomes of the comparison
data TContains t tu = TContains (t->tu)       -- injection function
		                (tu->Maybe t) -- projection function
data TNone

class TypeEqInc tu t res | tu t -> res where
    tsearch :: tu -> t -> res
    tsearch = undefined


instance TypeEqInc tu tu TSame
instance (IsEither tu atu, TypeEqInc' atu t res) 
    => TypeEqInc tu t res
  where
    tsearch tu = tsearch' (undefined::atu)

class TypeEqInc' atu t res | atu t -> res where
    tsearch' :: atu -> t -> res
    tsearch' = undefined

instance TypeEqInc' TNone t TNone
instance (TypeEqInc tul t atul, TypeEqInc tur t atur,
	  TypeEqInc'' atul atur tul tur res)
    => TypeEqInc' (Either tul tur) t res where
    tsearch' atu t = tsearch'' (tsearch (undefined::tul) t)
                               (tsearch (undefined::tur) t)
			       (undefined::tul)
			       (undefined::tur)

class TypeEqInc'' atul atur tul tur res | atul atur tul tur -> res where
    tsearch'' :: atul -> atur -> tul -> tur -> res
    tsearch'' = undefined

instance TypeEqInc'' TNone TNone tul tur TNone

instance TypeEqInc'' TSame TNone t tur (TContains t (Either t tur)) where
    tsearch'' _ _ _ _ = TContains Left (either Just (const Nothing))

instance TypeEqInc'' TNone TSame tul t (TContains t (Either tul t)) where
    tsearch'' _ _ _ _ = TContains Right (either (const Nothing) Just)

instance TypeEqInc'' (TContains t tul) TNone tul tur
    (TContains t (Either tul tur)) where
    tsearch'' (TContains inj prj)  _ _ _ = 
	TContains (Left . inj) (either prj (const Nothing))

instance TypeEqInc'' TNone (TContains t tur) tul tur
    (TContains t (Either tul tur)) where
    tsearch'' _ (TContains inj prj) _ _ = 
	TContains (Right . inj) (either (const Nothing) prj)



-- Check to see if t is an Either type
class IsEither t res | t -> res
instance IsEither (Either t1 t2) (Either t1 t2)
instance TypeCast res TNone => IsEither t res


-- A few tests of consEither

-- consEither should act as a regular cons for homogeneous lists
te1 = consEither () nilEither
te2 = consEither () (consEither () nilEither)
-- [(),()]

te3 = consEither True (consEither () nilEither)
-- [Left True,Right ()]

te4 = consEither 'a' (consEither True (consEither () nilEither))
-- [Left 'a',Right (Left True),Right (Right ())]

te5 = consEither () (consEither True (consEither () nilEither))
-- [Right (),Left True,Right ()]

te6 = consEither 'a' (consEither True (consEither False nilEither))
-- [Left 'a',Right True,Right False]

te7 = consEither True (consEither True (consEither () nilEither))
-- [Left True,Left True,Right ()]

-- We instantiate the look-up (HasField) class for records.
-- We assure that the constructed unions form reasonable intersection type.

instance (HasField l x v, HasField l y v) 
       => HasField l (Either x y) v 
 where
  hLookupByLabel l (Left x)  =  hLookupByLabel l x
  hLookupByLabel l (Right y) =  hLookupByLabel l y


-- Down-cast a value of a union type to a summand type.
-- Make sure that the summand type occurs once at least.

class DownCast u s
  where
    downCast :: u -> Maybe s

instance (TypeEqInc u s au, DownCast' au u s) => DownCast u s where
    downCast = downCast' (tsearch (undefined::u) (undefined::s))


class DownCast' au u s | au s -> u where
    downCast' :: au -> u -> Maybe s

instance DownCast' TSame s s where
    downCast' _ = Just

instance DownCast' (TContains s u) u s where
    downCast' (TContains _ prj) = prj


-- The following is deliberately omitted. That means attempting to
-- project a type that is not in the union should give a type error.
-- instance DownCast' TNone s s

{-
We could replace (implicit) TypeEq with the subsumption
predicate that returns HTrue if the record contains all the fields of
the targeted type. Another design choice: we may assume the label name
to determine the type of the corresponding method. That forces the
width subtyping and consistent use of names, at least within one union
type. In that case, we can process even records with polymorphic
fields.
-}

