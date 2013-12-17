module Main where

import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Process (runProcess, waitForProcess)
import System.IO (openFile, hClose, Handle, IOMode(..))

split :: Eq a => a -> [a] -> ([a], [a])
split sep list = case splitted of
                   [] -> ([], [])
                   [a] -> (a, [])
                   _ -> (splitted!!0, splitted!!1)
    where splitted = splitOn [sep] list

devNull :: IO Handle
devNull = openFile "/dev/null" ReadMode

runcmd :: FilePath -> [String] -> IO ()
runcmd executable args = do null <- fmap Just devNull
                            handle <- runProcess ("/usr/bin/" ++ executable) args Nothing Nothing Nothing null null
                            waitForProcess handle
                            return ()

processFile :: [String] -> (Integer, String) -> IO ()
processFile args (num, name) = do putStrLn $ "Processing " ++ filename
                                  runcmd "cdda2wav" (args ++ ["-t", show num, filename ++ ".wav"])
                                  runcmd "flac" [filename ++ ".wav"]
                                  runcmd "rm" [filename ++ ".inf", filename ++ ".wav"]
                                  runcmd "mv" [filename ++ ".wav.flac", filename ++ ".flac"]
                                  runcmd "metaflac" [ "--set-tag=tracknumber=" ++ show num
                                                    , "--set-tag=title=" ++ name
                                                    , filename ++ ".flac"
                                                    ]

    where filename = (if num < 10 then '0' : show num else show num)  ++ ". " ++ name

main :: IO ()
main = do (files, args) <- fmap (split "--") getArgs
          mapM_ (processFile args) $ zip [1..] files
          null <- devNull
          hClose null