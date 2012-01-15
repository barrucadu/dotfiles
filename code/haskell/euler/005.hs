-----------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Project Euler problem 5 -----------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
------ 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.                        ------
------ What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?                                 ------
-----------------------------------------------------------------------------------------------------------------------------------------------

module Main where

-- Generate the list of natural numbers
nats :: [Int]
nats = nats' 1
    where nats' n = [n] ++ nats' (n + 1)

-- Check a number has a given set of factors
hasfactors :: Int -> [Int] -> Bool
hasfactors _ [] = True
hasfactors val (f:fs) | mod val f == 0 = hasfactors val fs
                      | otherwise      = False

-- Find the smallest number which divides evenly by 1 to 20
euler005 :: Int
euler005 = head [x | x <- nats, hasfactors x [1..20]]