module Main where

-- imports

-- local imports
import PRF

import Test.QuickCheck


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


-- main
main :: IO()

main = do 
    quickCheck $ prop_PRF_zero zeroR
    quickCheck $ prop_PRF_succ succR
    quickCheck $ prop_PRF_pred predR
    quickCheck $ prop_PRF_id idR
    quickCheck $ prop_PRF_sg sgR