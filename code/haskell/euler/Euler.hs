---- Project Euler utility functions

module Euler where

-----------------------------------
---- Infinite lists of numbers ----
-----------------------------------

-- Generate the list of natural numbers
nats :: [Int]
nats = nats' 1
    where nats' n = [n] ++ nats' (n + 1)

-- Generate an infinite list of fibonacci numbers
fib :: [Int]
fib = [0, 1] ++ fib' 0 1
    where fib' a b = [a + b] ++ fib' b (a + b)

----------------------------
---- Filters and sieves ----
----------------------------

-- Filter a sorted list by some upper bound
filterupper :: (a -> Bool) -> [a] -> [a]
filterupper f []     = []
filterupper f (a:as) | f a       = [a] ++ filterupper f as
                     | otherwise = []

--------------------
---- Predicates ----
--------------------

-- Check a number has a given set of factors
hasfactors :: Int -> [Int] -> Bool
hasfactors _ [] = True
hasfactors val (f:fs) | mod val f == 0 = hasfactors val fs
                      | otherwise      = False

-- Check a number has any of some factors
hasAnyFactors :: Int -> [Int] -> Bool
hasAnyFactors _ []       = False
hasAnyFactors val (f:fs) | mod val f == 0 = True
                         | otherwise      = hasAnyFactors val fs

---------------------------
---- Numeric functions ----
---------------------------

-- Square a number
square :: Int -> Int
square a = a * a
