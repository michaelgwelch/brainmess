module Main where

import Program 
import System
import Tape

main :: IO ()
main = do
            args <- getArgs
            prog <- readFile $ args !! 0
            run (createProgram prog) tape
            putStrLn "";
            return ()
