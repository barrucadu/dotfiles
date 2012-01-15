-----------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------- Project Euler problem 5 -----------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
------ 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.                        ------
------ What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?                                 ------
-----------------------------------------------------------------------------------------------------------------------------------------------

module Main where

import Euler

-- Find the smallest number which divides evenly by 1 to 20
euler005 :: Int
euler005 = head [x | x <- nats, hasfactors x [1..20]]