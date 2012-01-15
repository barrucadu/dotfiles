-----------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Project Euler problem 1 -----------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
------ If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. ------
------ Find the sum of all the multiples of 3 or 5 below 1000.                                                                           ------
-----------------------------------------------------------------------------------------------------------------------------------------------

module Main where

-- Get a list of all multiples of a list of factors in a given range
multiples :: Int -> Int -> [Int] -> [Int]
multiples start end factors | end < start = []
                            | otherwise   = [start | hasfactor start factors ] ++ multiples (start + 1) end factors

                            where hasfactor _ [] = False
                                  hasfactor val (f:fs) | mod val f == 0 = True
                                                       | otherwise      = hasfactor val fs

-- Sum the multiples of 3 or 5 from 0 to 1000
euler001 = sum (multiples 0 999 [3, 5])