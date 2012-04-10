Dice Functions
==============

Petals Around the Rose is based around rolling dice and figuring out what they
mean, therefore it is sensible to have a module specifically for dealing with
dice.

> module Dice where
> import System.Random

Firstly, we need a data type for dice.

> data Die = One | Two | Three | Four | Five | Six

Once we have that, we can start to display a die using unicode.

> instance Show Die where
>     show One   = "⚀"
>     show Two   = "⚁"
>     show Three = "⚂"
>     show Four  = "⚃"
>     show Five  = "⚄"
>     show Six   = "⚅"

Now, we need a die rolling function.

> rollDie :: IO Die
> rollDie = do roll <- getStdRandom $ randomR (1, 6)
>              return $ intToDie roll

We use a helper function, intToDie which takes the randomly generated int (in 1
to 6) and produce a DieRoll value.

>           where intToDie :: Int -> Die
>                 intToDie 1 = One
>                 intToDie 2 = Two
>                 intToDie 3 = Three
>                 intToDie 4 = Four
>                 intToDie 5 = Five
>                 intToDie 6 = Six

And now, a convenience function to generate a list of die rolls.

> rollDice :: Int -> IO [Die]
> rollDice n = sequence $ rollDice' n
>              where rollDice' 0 = []
>                    rollDice' n = rollDie : rollDice' (n - 1)
