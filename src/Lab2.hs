module Main where

-- imports

-- local imports
import PRF
-- Global imports
import Test.QuickCheck ( quickCheck )

------------------------------------------------------------------------------
-- Properties as functions, to check if the implemeted functions return the
-- same result you where searching, there is one property for each PRF.
------------------------------------------------------------------------------
-- All the implemeted properties return a boolean, and they are supposed to
-- compare if the basic function is equal to the PRF with recNat, and
-- if they are, then is True

prop_PRF_zero :: (Nat -> Nat) -> Nat -> Bool
prop_PRF_zero f1 n = zeroP n == f1 n

prop_PRF_succ :: (Nat -> Nat) -> Nat -> Bool
prop_PRF_succ f1 n = succP n == f1 n

prop_PRF_pred :: (Nat -> Nat) -> Nat -> Bool
prop_PRF_pred f1 n = predP n == f1 n

prop_PRF_id :: (Nat -> Nat) -> Nat -> Bool
prop_PRF_id f1 n = idP n == f1 n

prop_PRF_sg:: (Nat -> Nat) -> Nat -> Bool
prop_PRF_sg f1 n = sgP n == f1 n

------------------------------------------------------------------------------
-- main
main :: IO()

main = do
    -- Running tests
    quickCheck $ prop_PRF_zero zeroR
    quickCheck $ prop_PRF_succ succR
    quickCheck $ prop_PRF_pred predR
    quickCheck $ prop_PRF_id idR
    quickCheck $ prop_PRF_sg sgR