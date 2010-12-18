module Main where

import System
import System.Environment.Executable
import System.Directory
import Data.List
import Types
import Read
import Eval
import System.Console.Haskeline
import System.Console.Haskeline.IO
import ReadFile


version = "0.1"
-- check for -v or -i flags
vFlag [] = False
vFlag (h:t) = if h == "-v" then True else vFlag t
iFlag [] = False
iFlag (h:t) = if h == "-i" then True else iFlag t

main = do args <- getArgs
          state <- initializeInput defaultSettings
          let verbose = vFlag args
          let interpret = iFlag args
          -- import std.lib
          bindings <- importFile verbose 0 ["std", "lib"]
          unscoped <- case bindings of
                        (False, _) -> do putStrLn "Failed to import std.lib."
                                         return []
                        (True, b) -> do return $ unscope b
          if verbose then putStrLn "-v Verbose mode on" else return ()
          if (length args) > 0 && isSuffixOf ".sco" (args !! 0) 
            -- if a .sco filename is given as the first argument, interpret that file
            then do newbindings <- execute verbose (args !! 0) unscoped
                    -- if the -i flag is set, start the interpreter
                    if interpret then loop verbose (unscope newbindings) state
                                 else return ()
            -- otherwise, start the interpreter
            else do putStrLn ("Scotch interpreter, version " ++ version)                    
                    loop verbose unscoped state

-- the interpreter's main REPL loop
loop :: Bool -> [Binding] -> InputState -> IO ()
loop verbose bindings state = 
  do line <- queryInput state (getInputLine ">> ")
     case line of
        Nothing -> return ()
        Just "quit" -> closeInput state
        Just input -> do -- parse input
                         let parsed = snd $ head $ (Read.read "Interpreter" input)
                         imp' <- case parsed of
                                   Import s -> importFile verbose 0 s
                                   otherwise -> do return (False, [(0, (Pattern (Bit False), ([], Val $ Bit False)))])
                         imp <- case imp' of
                                  (False, []) -> do -- the imported module failed to open
                                                    putStrLn ("Failed to open " ++ show (parsed))
                                                    return []
                                  (False, f) -> do -- there was no attempted import
                                                   return []
                                  (True, b) -> do -- successful module import
                                                  return b
                         -- evaluate parsed input
                         let result = eval parsed bindings
                         if verbose then putStrLn (show parsed)
                                    else return ()
                         -- determine whether any definitions were made
                         let newBindings = case parsed of
                                             Def id x Placeholder -> [(id, ([], x))]
                                             EagerDef id x Placeholder -> [(id, ([], (case eval x bindings of
                                                                                        Result r -> Val r
                                                                                        Exception s -> Undefined s
                                                                                      )))]
                                             Defun id params x Placeholder -> [(id, (params, x)), (id, ([], Val (HFunc id)))]
                                             otherwise -> []
                         -- output, if necessary
                         case parsed of
                           Output x y -> case (eval x bindings) of
                                           Result (Str s) -> putStrLn s
                                           Incomplete i -> return ()
                                           e -> putStrLn (show e)
                           otherwise -> case result of
                                          Incomplete i -> return ()
                                          otherwise -> putStrLn (show result)
                         -- continue loop
                         loop verbose (newBindings ++ (unscope imp) ++ bindings) state
