{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import System
import System.Environment.Executable
import System.Directory
import Data.List
import System.Console.Haskeline
import System.Console.Haskeline.IO
import ReadFile
import Types
import Bindings
import Read
import Eval
import Substitute


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
        Just "restart" -> main
        Just "vars" -> do putStrLn (show bindings)
                          loop verbose bindings state
        Just input -> do -- parse input
                         let readinput = Read.read "Interpreter" input
                         parsed <- subfile (case readinput of 
                                                 [] -> Skip
                                                 otherwise -> snd $ head $ (readinput)) bindings                         
                         imp' <- case parsed of
                                   Import s -> importFile verbose 1 s
                                   otherwise -> do return (False, [(1, (Pattern (Bit False), ([], Val $ Bit False)))])
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
                         newBindings <- case parsed of
                                          Def id x Placeholder -> do return [(id, ([], x))]
                                          EagerDef id x Placeholder -> do return [(id, ([], (case eval x bindings of
                                                                                              Result r -> Val r
                                                                                              Exception s -> Undefined s
                                                                                             )))]
                                          Defun id params x Placeholder -> do return [(id, (params, x)), (id, ([], Val (HFunc id)))]
                                          Defproc id params x Placeholder -> do return [(id, (params, Val (Proc x))), (id, ([], Val (HFunc id)))]
                                          otherwise -> case result of
                                                         Result (Proc p) -> do new <- (wexecute verbose 
                                                                                                [(Nothing, e) | e <- p] 
                                                                                                (rescope bindings))
                                                                               return $ unscope new
                                                         otherwise -> do return []
                         -- output, if necessary
                         case result of
                           Incomplete i -> return ()
                           Result (Proc p) -> return ()
                           PrintOutput p -> putStrLn p
                           FileOutput f x -> writeFile f x
                           FileOutputA f x -> appendFile f x
                           otherwise -> putStrLn (show result)
                         -- continue loop
                         loop verbose (unscope (addBindings ((rescope newBindings) ++ imp) (rescope bindings))) state
