{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

The concept of subtyping. 

We keep track of the OO inheritance hierarchy by means of a dedicated
Haskell class Subtype. As an aside, one may actually argue that this
approach is less about inheritance; it is perhaps closer to
delegation.

The Subtype class hosts two methods for applying observers vs.\
readers to objects. This is an essential convenience layer for
defining accessors (getters/setters) on object types such that they
also work on derived types.

The use of a two-parameter type-class is not really essential.  In
Haskell 98, we would simply specialise this type-class per OO class.
MP Jones and SP Jones adopt this idea in "OO style overloading for
Haskell", even though they do not consider state.

Note that there is reflexive instance for subtyping. It would be
possible to eliminate the use of a generic instance.  That is we could
use specific instances per actual OO class.  Transitivity (along
subtyping chains) is not so easily taken care of. For each new type,
we need to provide instances for all ancestors.

-}

module Subtype where

infix 7 .?. -- observation
infix 7 .!. -- mutation

class Subtype a b where
 (.?.) :: (b -> r) -> a -> r
 (.!.) :: (b -> b) -> a -> a

instance Subtype a a where
 (.?.) = id
 (.!.) = id
