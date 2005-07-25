
-- Safe MFix to prevent staging errors

module SMRFix (NotConstructed, smrfix, srret) where

import Control.Monad.Fix

-- the data constructor is not exported!
newtype NotConstructed a = NotConstructed a

-- Only the following two deconstruct the NotConstructed object
-- The following two constitute the security kernel

smrfix :: (MonadFix m) => (NotConstructed a -> m (NotConstructed a)) -> m a
smrfix f = mfix f >>= (\ (NotConstructed a) -> return a)

-- To be absolutely fool-proof, we should require that a is not of the type
-- of IO b. Although if it is, the method call needs a special `monadic
-- join'. Anyway, we can prevent a from being IO b using typeclasses.
-- Or using the 's' parameter as in ST monad and as in MetaOCaml.
-- But these are just details.

srret :: NotConstructed a -> (a-> b) -> IO (NotConstructed b)
srret (NotConstructed self) f = return $ NotConstructed (f self)
