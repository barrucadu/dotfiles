-----------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Project Euler problem 6 -----------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
------ The sum of the squares of the first ten natural numbers is,                                                                       ------
------ 1² + 2² + ... + 10² = 385                                                                                                         ------
------ The square of the sum of the first ten natural numbers is,                                                                        ------
------ (1 + 2 + ... + 10)² = 55² = 3025                                                                                                  ------
------ Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is                 ------
------ 3025 - 385 = 2640                                                                                                                 ------
------ Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.            ------
-----------------------------------------------------------------------------------------------------------------------------------------------

module Main where

-- Generate the list of natural numbers
nats :: [Int]
nats = nats' 1
    where nats' n = [n] ++ nats' (n + 1)

-- Filter a sorted list by some upper bound
filterupper :: (a -> Bool) -> [a] -> [a]
filterupper f []     = []
filterupper f (a:as) | f a       = [a] ++ filterupper f as
                     | otherwise = []

-- Square a number
square :: Int -> Int
square a = a * a

-- Find the difference between the sum of the squares and the square of the sum of 1 to 100
euler006 :: Int
euler006 = square (sum nats100) - sum (map square nats100)
    where nats100 = filterupper (\a -> a <= 100) nats