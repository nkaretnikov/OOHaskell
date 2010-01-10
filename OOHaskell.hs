{-

OOHaskell (C) 2004--2010, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This module gathers the API that we need for OOP in Haskell.  We
basically select a certain configuration of the HList library, and we
also import modules that are needed for mutable data and monads.

-}


module OOHaskell (

 module Data.HList.CommonMain,
 module Data.HList.GhcSyntax,
 module Data.HList.GhcRecord,
 module Data.HList.GhcExperiments,
 module Data.HList.MakeLabels,
 module Data.STRef,
 module Data.IORef,
 module Data.Dynamic,
 module Control.Monad,
 module Control.Monad.ST,
 module Control.Monad.Fix,
 returnIO,
 module Print,
 module DeepNarrow,
 module Nominal,
 module Dynamic,
 module New,
 module OOHaskell

) where


import Data.HList.CommonMain hiding ( HDeleteMany
                         , hDeleteMany
                         , TypeCast -- no external uses are valid; see TypeEqGeneric1
                         , typeCast -- no external uses are valid; see TypeEqGeneric1
                         )

import Data.HList.GhcSyntax hiding (( .*. ), ( :=: ))
import Data.HList.GhcRecord
import Data.HList.GhcExperiments
import Data.HList.TypeEqBoolGeneric
import Data.HList.FakePrelude
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1
import Data.HList.Label4
import Data.HList.MakeLabels
import Print
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
