module PRF ( Nat(..), zeroR, zeroP, succR, succP, idR, idP, predR, predP, sgR, sgP ) where

-- libraries imports
import Numeric.Natural ( Natural )
import Test.QuickCheck ( Arbitrary (..), arbitrarySizedNatural, shrink)

-- data structures
-- This mean that natural numbers are represented in a linked list, and
-- the type could be either Zero or Natural succesor
data Nat = Zero | Succ Nat
    deriving ( Eq , Show )

------------------------------------------------------------------------------
-- basic funtions
--This function fodls over a Nat, and 

recNat :: a -> ( Nat -> a -> a ) -> Nat -> a
-- In here recNat just ignores the second argumente and returns a
recNat a _ Zero = a
-- recNat a h counts gradually to the final zero, calling h on every 
-- intermediate Succ and replacing Zero with a
recNat a h ( Succ n ) = h n ( recNat a h n )

------------------------------------------------------------------------------

-- Fuction implementations
-- We define a basic PRF and then we define it again with recNat, in that way 
-- when using quickCheck we could compare both results and see if they are equal

------------------------------------------------------------------------------
--Most of the implemented PRF recNat fuctions ignore their first argument, and 
-- return what the function is supposed  to return, and all of them have type
-- Nat -> Nat
------------------------------------------------------------------------------
-- Zero funtion
zeroP :: Nat -> Nat
zeroP _ = Zero

zeroR :: Nat -> Nat
zeroR n = recNat Zero (\ _ _ -> Zero) n

-- succesor funtion
succP :: Nat -> Nat
succP n = Succ n

succR :: Nat -> Nat
succR n = recNat n (\ _ y -> Succ y ) (Succ Zero)

-- identity function
idP :: Nat -> Nat
idP n = n

idR :: Nat -> Nat
idR n = recNat n (\ _ y -> y ) Zero

-- predecesor funtion
predP :: Nat -> Nat
predP Zero = Zero
predP (Succ n) =  n

predR :: Nat -> Nat
predR Zero = Zero
predR (Succ n) = recNat n (\ _ y -> y) Zero

-- signum function
sgP :: Nat -> Nat
sgP Zero = Zero
sgP _ = Succ Zero

sgR :: Nat -> Nat
sgR Zero = recNat Zero (\ _ _ -> Zero) Zero
sgR _ = recNat Zero (\ n _ -> Succ n) (Succ Zero)

------------------------------------------------------------------------------
-- QuickCheck requiriments
fromNatural :: Natural -> Nat
fromNatural 0 = Zero
fromNatural n = Succ ( fromNatural $ pred n )

instance Arbitrary Nat where
    arbitrary = fmap fromNatural arbitrarySizedNatural
    shrink Zero = []
    shrink ( Succ n ) = n : shrink n

------------------------------------------------------------------------------




