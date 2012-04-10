Petals Around the Rose
======================

Petals Around the Rose is a lateral thinking puzzle game. The aim of the game is
to figure out how the result of each roll is calculated. Do not read this code
unless you already know the rule, or don't mind the puzzle being spoiled for
you.

> module Main where
> import System.Random

Die-related Functions
---------------------

The game is about dice, and so rather than displaying numbers to the user, let's
display dice. Firstly, we need a data type for dice.

> data DieRoll = One | Two | Three | Four | Five | Six

Once we have that, we can start to display a die using unicode.

> showDie :: DieRoll -> String
> showDie One   = "⚀"
> showDie Two   = "⚁"
> showDie Three = "⚂"
> showDie Four  = "⚃"
> showDie Five  = "⚄"
> showDie Six   = "⚅"

Now, we need a die rolling function.

> rollDie :: IO DieRoll
> rollDie = do roll <- getStdRandom $ randomR (1, 6)
>              return $ intToDie roll

We use a helper function, intToDie which takes the randomly generated int (in 1
to 6) and produce a DieRoll value.

>           where intToDie :: Int -> DieRoll
>                 intToDie 1 = One
>                 intToDie 2 = Two
>                 intToDie 3 = Three
>                 intToDie 4 = Four
>                 intToDie 5 = Five
>                 intToDie 6 = Six

And now, convenience functions to generate a list of dice rolls, and display them.

> showDice :: [DieRoll] -> String
> showDice [d]    = showDie d
> showDice (d:ds) = showDie d ++ " " ++ showDice ds

> rollDice :: Int -> IO [DieRoll]
> rollDice n = sequence $ rollDice' n
>              where rollDice' 0 = []
>                    rollDice' n = rollDie : rollDice' (n - 1)

Puzzle Logic
------------

Warning, the solution to the game is in the following function!

Now we need to calculate how many petals around the rose there are for a given
list of dice rolls.

> petals :: [DieRoll] -> Int
> petals [] = 0
> petals (d:ds) = petals ds + case d of
>                               One   -> 0
>                               Two   -> 0
>                               Three -> 2
>                               Four  -> 0
>                               Five  -> 4
>                               Six   -> 0

We also need a function to take a player guess and check if it's correct.

> try :: Int -> [DieRoll] -> Bool
> try g r = g == (petals r)

> guess :: [DieRoll] -> IO Bool
> guess r = do putStrLn "Enter your guess: "
>              g <- getInt
>              return $ try g r

Tying it all together
---------------------

Now we just need to, upon program load, roll five dice, print them, wait for the
player to make a guess, inform them if their guess is correct or not, and if not
print the actual result.

> main :: IO ()
> main = do roll <- rollDice 5
>           putStrLn (showDice roll)

Now get a guess from the user…

>           ans <- guess roll

And now we know if they were correct or not, and so can respond accordingly.

>           case ans of
>             True  -> putStrLn "Well done!"
>             False -> putStrLn ("Actually, the result is " ++ show (petals roll))

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
