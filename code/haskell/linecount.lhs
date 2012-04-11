LineCount
=========

Tool to check how long each line of a file is.

> main :: IO ()
> main = checkLine 1
>     where checkLine n = do line <- getLine
>                            let len = length line
>
>                            putStr $ if len > 80 
>                              then "Length of line " ++ show n ++ 
>                                    " is "  ++ show len ++ "\n"
>                              else ""
>
>                            checkLine (n + 1)
