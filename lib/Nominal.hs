{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-

-- (C) 2004-2010, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

Support nominal subtyping.

-}


module Nominal (
 module Nominal
) where

import Data.HList.FakePrelude
import Data.HList.HList
import Data.HList.HListPrelude
import Data.HList.HOccurs
import Data.HList.Record


-- A newtype wrapper for nominal types

newtype Nomination n => N n r = N r


--
-- The presentation of the nominal subtyping hierarchy.
-- Define the parents of each nominal type by an HList.
--

class (Nomination child, Nominations parents) 
   => Parents child parents | child -> parents


-- Lists of nominations

class Nominations ns
instance Nominations (HList '[])
instance (Nomination h, Nominations (HList t))
  => Nominations (HList (h ': t))


-- A class for nominal types

class Nomination n
instance (Parents n ns, Nominations ns)
      => Nomination n


-- An operation to `nominate' a record as nominal object

nominate ::  Nomination n => n -> r -> N n r
nominate n = N


-- An operation to `anonymize' a nominal object

anonymize ::  Nomination n => N n r -> r
anonymize (N r) = r


-- For method look-up. It should not overlap with anything else

instance ( HasField l r v
         , Nomination n
         )
           => HasField l (N n r) v
 where hLookupByLabel l r = hLookupByLabel l (anonymize r)


-- Test whether g is an ancestor class of f

class (Nomination f, Nomination g)
   => Ancestor f g


-- Compute all ancestors and perform membership test

instance ( Nomination f
         , Nomination g
         , Ancestors (HList (f ': '[])) hs
         , HOccurs g hs
         ) => Ancestor f g


-- Compute transitive closure of ancestors

class ( Nominations fs
      , Nominations gs
      )
        => Ancestors fs gs | fs -> gs


-- No more ancestors

instance Ancestors (HList '[]) (HList '[])


-- Append ancestors
class HAppend a b => HAppend3 a b c | a b -> c

instance ( Parents h l1
         , Ancestors l1 l2
         , Ancestors (HList t) l3
         , HAppend3 l2 l3 l4
         , HAppend3 l4 (HList (h ': '[])) l0
         , Nominations l0
         ) => Ancestors (HList (h ': t)) l0


-- Guard the nomination of an operand

hasNomination :: N n x -> n -> N n x
hasNomination o _ = o


-- An up-cast operation

nUpCast :: Ancestor n n' => N n x -> N n' x
nUpCast = N . anonymize


-- An up-cast operation with target type

nUpCastTo :: Ancestor n n' => N n x -> n' -> N n' x
nUpCastTo x n' = nUpCast x
