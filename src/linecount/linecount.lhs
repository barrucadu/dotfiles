LineCount
=========

The maximum length of a line.

> maxlen = 80

Read each line from stdin and print out an error if it is longer than the
maximum length.

> main :: IO ()
> main = checkLine 1
>     where checkLine n = do line <- getLine
>                            let len = length line
>
>                            putStr $ if len > maxlen
>                              then "Length of line " ++ show n ++ 
>                                    " is "  ++ show len ++ "\n"
>                              else ""
>
>                            checkLine (n + 1)
