{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

-- Safe mfix to prevent staging errors
-- NotFixed *data* constructor is not exported

-}


module New (NotFixed, new, construct, constructWithSuper) where

import Control.Monad.Fix
import Data.HList.Record


-- The data constructor is not exported!

newtype NotFixed a = NotFixed a


--
-- Only the following two primitives deconstruct the NotFixed term.
-- These primitives constitute the security kernel.
--

new :: (NotFixed (Record a) -> IO (NotFixed (Record a))) 
    -> IO (Record a)

new f = mfix f >>= (\(NotFixed a) -> return a)


--
-- Note that f has not a monadic type.
-- So during the execution of f, effects are not possible.
-- Except non-termination.
--

construct :: NotFixed (Record a)
          -> (Record a -> Record b)
          -> IO (NotFixed (Record b))

construct (NotFixed self) f = return $ NotFixed (f self)


--
-- We also need a version that takes into account a super.
-- For multiple inheritance/object composition we would need yet another version.
-- It's straightforward to use a type class to this end.
-- Working title: "constructWithParents".
--

constructWithSuper :: NotFixed (Record super)
                   -> NotFixed (Record self)
                   -> (Record super -> Record self -> Record this)
                   -> IO (NotFixed (Record this))

constructWithSuper (NotFixed super)
                   (NotFixed self)
		   f
 = 
	return $ NotFixed (f super self)
