{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

Support nominal subtyping.

-}


module Nominal (
 module Nominal
) where

import Data.HList.FakePrelude
import Data.HList.HListPrelude
import Data.HList.HOccurs
import Data.HList.Record


-- A newtype wrapper for nominal types

newtype N nom rec = N rec


-- A class for nominal types

class Nomination f


-- An operation to `nominate' a record as nominal object

nominate ::  Nomination nt => nt -> x -> N nt x
nominate nt x = N x


-- An operation to `anonymize' a nominal object

anonymize ::  Nomination nt => N nt x -> x
anonymize (N x) = x


-- For method look-up. It should not overlap with anything else

instance (HasField l x v, Nomination f) => HasField l (N f x) v
 where hLookupByLabel l o = hLookupByLabel l (anonymize o)


--
-- The presentation of the nominal inheritance hierarchy
-- Define which nominal type is a parent for which type.
--

class ( Nomination child
      , Nominations parents
      ) 
        => Parents child parents | child -> parents


-- Lists of nominations

class Nominations ns
instance Nominations HNil
instance ( Nomination h
         , Nominations t
         )
           => Nominations (HCons h t)


-- Test whether g is an ancestor class of f

class ( Nomination f
      , Nomination g
      )
        => Ancestor f g


-- Compute all ancestors and perform membership test

instance ( Nomination f
         , Nomination g
         , Ancestors (HCons f HNil) hs
         , HOccurs g hs
         )
           => Ancestor f g


-- Compute transitive closure of ancestors

class ( Nominations fs
      , Nominations gs
      )
        => Ancestors fs gs | fs -> gs


-- No more ancestors

instance Ancestors HNil HNil


-- Append ancestors

instance ( Parents h l1
         , Ancestors l1 l2
         , Ancestors t l3
         , HAppend l2 l3 l4
         , HAppend l4 (HCons h HNil) l0
         , Nominations l0
         )
           => Ancestors (HCons h t) l0


-- An up-cast operation

nUpCast :: Ancestor f g => N f x -> N g x
nUpCast = N . anonymize


-- An up-cast operation with target type

nUpCastTo :: Ancestor f g => N f x -> g -> N g x
nUpCastTo x (nt::nt) = nUpCast x
