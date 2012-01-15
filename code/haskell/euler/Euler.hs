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
filterUpper :: (a -> Bool) -> [a] -> [a]
filterUpper f []     = []
filterUpper f (a:as) | f a       = [a] ++ filterUpper f as
                     | otherwise = []

--------------------
---- Predicates ----
--------------------

-- Check a number has a given set of factors
hasFactors :: Int -> [Int] -> Bool
hasFactors _ [] = True
hasFactors val (f:fs) | mod val f == 0 = hasFactors val fs
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
