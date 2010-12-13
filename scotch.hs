module Main where

import System
import Types
import Read
import Eval
import System.Console.Haskeline

version = "0.1"
vFlag [] = False
vFlag (h:t) = if h == "-v" then True else vFlag t

main = do putStrLn ("Scotch interpreter, version " ++ version)
          args <- getArgs
          let verbose = vFlag args
          runInputT defaultSettings (loop verbose)
loop :: Bool -> InputT IO ()
loop verbose = do line <- getInputLine ">> "
                  case line of
                    Nothing -> return ()
                    Just "quit" -> return ()
                    Just input -> do let parsed = Read.read input
                                     let result = eval parsed
                                     if verbose then outputStrLn (show parsed)
                                                else return ()
                                     outputStrLn (show result)
                                     loop verbose
