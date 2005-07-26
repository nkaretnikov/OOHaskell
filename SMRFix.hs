{-# OPTIONS -fglasgow-exts #-}

-- Safe MFix to prevent staging errors
-- NotConstructed *data* cosntructor is not exported 
module SMRFix (NotConstructed, smrfix, srret) where

import Control.Monad.Fix
import OOHaskell (Record)

-- the data constructor is not exported!
newtype NotConstructed a = NotConstructed a

-- Only the following two deconstruct the NotConstructed object
-- The following two constitute the security kernel

smrfix :: (MonadFix m) => 
	  (NotConstructed (Record a) -> m (NotConstructed (Record a))) 
	  -> m (Record a)
smrfix f = mfix f >>= (\ (NotConstructed a) -> return a)


-- Note that f has not a monadic type, so during the execution of
-- f, effects (except non-termination) are not possible.

class SRRet a na | a -> na where
    srret :: a -> (na -> (Record b)) -> IO (NotConstructed (Record b))

instance SRRet (NotConstructed (Record a)) (Record a) where
    srret (NotConstructed self) f = return $ NotConstructed (f self)

instance SRRet ((NotConstructed (Record a)), (NotConstructed (Record b)))
               (Record a, Record b) where
	
    srret ((NotConstructed s1), (NotConstructed s2)) f = 
	return $ NotConstructed (f (s1,s2))

