module Main where

import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  
  path <- 
    case args of
      path:[] -> return path
      _       -> die "Usage: importusers FILENAME"

  rows <- lines <$> readFile path

  putStrLn $ unlines rows
