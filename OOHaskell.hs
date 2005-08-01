{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This module gathers the API that we need for OOP in Haskell.  We
basically select a certain configuration of the HList library, and we
also import modules that are needed for mutable data and monads. Note
on overlapping: Needed for the chosen model of labels. Other models
can be used instead, but the chosen look better in types.

-}


module OOHaskell (

 module CommonMain,
 module GhcSyntax,
 module GhcRecord,
 module GhcExperiments,
 module Data.STRef,
 module Data.IORef,
 module Data.Typeable,
 module Control.Monad,
 module Control.Monad.ST,
 module Control.Monad.Fix,
 module GHC.IOBase,
 module DeepNarrow,
 module OOHaskell

) where


import CommonMain hiding ( HDeleteMany
                         , hDeleteMany
                         , TypeCast
                         , typeCast
                         )

import GhcSyntax hiding (( .*. ), ( :=: ))
import GhcRecord
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import Label4
import DeepNarrow

import Data.STRef
import Data.IORef
import Data.Typeable
import Control.Monad
import Control.Monad.ST
import Control.Monad.Fix
import GHC.IOBase hiding (stToIO, writeIORef, readIORef, newIORef, IORef,unsafeIOToST)

infixr 9 #
m # field = (m .!. field) 

infixr 2 .*.
(l,v) .*. (Record r) = mkRecord (HCons (l,v) r)

infixr 4 :=:
type l :=: v = (Proxy l,v)

concrete generator self = generator self
 where
  _ = mfix generator
