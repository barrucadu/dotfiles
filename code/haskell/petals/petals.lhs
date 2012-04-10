Petals Around the Rose
======================

Petals Around the Rose is a lateral thinking puzzle game. The aim of the game is
to figure out how the result of each roll is calculated. Do not read this code
unless you already know the rule, or don't mind the puzzle being spoiled for
you.

> module Main where
> import Dice

Puzzle Logic
------------

Warning, the solution to the game is in the following function!

Now we need to calculate how many petals around the rose there are for a given
list of dice rolls.

> petals :: [Die] -> Int
> petals [] = 0
> petals (d:ds) = petals ds + case d of
>                               One   -> 0
>                               Two   -> 0
>                               Three -> 2
>                               Four  -> 0
>                               Five  -> 4
>                               Six   -> 0

Tying it all together
---------------------

Now we just need to, upon program load, roll five dice, print them, wait for the
player to make a guess, inform them if their guess is correct or not, and if not
print the actual result.

> main :: IO ()
> main = do putStrLn "1. The name, Petals Around the Rose, is significant."
>           putStrLn "2. The answer is always an even number."
>           putStrLn "3. You will be given the answers, but not the reasoning."

Now, roll the dice and get the actual result.

>           roll <- rollDice 5
>           let p = petals roll
>           putStrLn $ show roll

Now get a guess from the user…

>           putStrLn "Enter your guess: "
>           g <- getInt

And now we know if they were correct or not, and so can respond accordingly.

>           case g == p of
>             True  -> putStrLn "Well done!"
>             False -> putStrLn $ "Actually, the result is " ++ show p

Utilities
---------

Above in the guess function, we used a getInt method, which reads a line and
returns an IO Int. There is no such method, so we have to make one.

Unfortunately, the current implementation dies when something other than an int
is entered. I don't know enough Haskell yet to do validation on user input and
try again.

> getInt :: IO Int
> getInt = do line <- getLine
>             return $ read line
