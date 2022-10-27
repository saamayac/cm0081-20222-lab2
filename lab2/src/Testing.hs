module Testing where

-- | imports
import Numeric.Natural ( Natural )
import Test.QuickCheck ( Arbitrary (..)
                        , arbitrarySizedNatural
                        , shrink )

-- data structures
data Nat = Zero | Succ Nat
    deriving ( Eq , Show )

-- basic funtions
fromNatural :: Natural -> Nat
fromNatural 0 = Zero
fromNatural n = Succ ( fromNatural $ pred n )

recNat :: a -> ( Nat -> a -> a ) -> Nat -> a
recNat a _ Zero = a
recNat a h ( Succ n ) = h n ( recNat a h n )

------------------------------------------------------------------------------
-- addtition

-- Normal funtion
addP :: Nat -> Nat -> Nat
addP Zero n = n
addP ( Succ m ) n = Succ ( addP m n )

-- fution with recNat
addR :: Nat -> Nat -> Nat
addR m n = recNat n (\ _ y -> Succ y ) m


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
predR (Succ n) = recNat n (\ _ (Succ y) -> y) Zero

-- signum function
sgP :: Nat -> Nat
sgP Zero = Zero
sgP _ = Succ Zero

sgR :: Nat -> Nat
sgR Zero = recNat Zero (\ n _ -> Zero) Zero
sgR _ = recNat Zero (\ n _ -> Succ n) (Succ Zero)



------------------------------------------------------------------------------
-- QuickCheck requiriments
instance Arbitrary Nat where
arbitrary = fmap fromNatural arbitrarySizedNatural
shrink Zero = []
shrink ( Succ n ) = n : shrink n

prop_PRF f1 f2 = f1 && f2
------------------------------------------------------------------------------

-- main
main :: IO()

main = do
    quickCheck (prop_PRF zeroR zeroP)