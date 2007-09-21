{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004--2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This module gathers the API that we need for OOP in Haskell.  We
basically select a certain configuration of the HList library, and we
also import modules that are needed for mutable data and monads. Note
on overlapping: merely needed for the chosen model of labels.

-}


module OOHaskell (

 module CommonMain,
 module GhcSyntax,
 module GhcRecord,
 module GhcExperiments,
 module MakeLabels,
 module Data.STRef,
 module Data.IORef,
 module Data.Dynamic,
 module Control.Monad,
 module Control.Monad.ST,
 module Control.Monad.Fix,
 returnIO,
 module DeepNarrow,
 module Nominal,
 module Dynamic,
 module New,
 module OOHaskell

) where


import CommonMain hiding ( HDeleteMany
                         , hDeleteMany
                         , TypeCast -- no external uses are valid; see TypeEqGeneric1
                         , typeCast -- no external uses are valid; see TypeEqGeneric1
                         )

import GhcSyntax hiding (( .*. ), ( :=: ))
import GhcRecord
import GhcExperiments
import TypeEqBoolGeneric
import FakePrelude
import TypeEqGeneric1
import TypeCastGeneric1
import Label4
import MakeLabels
import DeepNarrow
import Nominal
import Dynamic
import New

import Data.STRef
import Data.IORef
import Data.Typeable
import Data.Dynamic
import Control.Monad
import Control.Monad.ST
import Control.Monad.Fix

import GHC.IOBase (returnIO)

infixr 9 #
m # field = (m .!. field) 

infixr 2 .*.
f@(LVPair v) .*. (Record r) = mkRecord (HCons f r)

infixr 4 :=:
type l :=: v = LVPair (Proxy l) v

concrete generator self = generator self
 where
  _ = mfix generator

nil = nilLub
cons h t = consLub h t
