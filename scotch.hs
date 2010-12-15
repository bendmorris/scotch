module Main where

import System
import System.Environment.Executable
import System.Directory
import Data.List
import Types
import Read
import Eval
import System.Console.Haskeline
import ReadFile


version = "0.1"
vFlag [] = False
vFlag (h:t) = if h == "-v" then True else vFlag t
iFlag [] = False
iFlag (h:t) = if h == "-i" then True else iFlag t

unscope :: [ScopedBinding] -> [Binding]
unscope [] = []
unscope (h:t) = (snd h) : unscope t

left [] 0 = []
left (h:t) 0 = []
left (h:t) n = h : (left t (n - 1))

main = do args <- getArgs
          let verbose = vFlag args
          let interpret = iFlag args
          full_path <- splitExecutablePath
          let path = (fst full_path) ++ "scotch.std/lib.sco"
          exists <- doesFileExist path
          bindings <- case exists of 
                        True -> execute verbose path []
                        False -> do return []
          let unscoped = unscope bindings
          if verbose then putStrLn "-v Verbose mode on" else return ()
          if (length args) > 0 && isSuffixOf ".sco" (args !! 0) 
            then do newbindings <- execute verbose (args !! 0) unscoped
                    if interpret then runInputT defaultSettings (loop verbose (unscope newbindings))
                                 else return ()
            else do putStrLn ("Scotch interpreter, version " ++ version)                    
                    runInputT defaultSettings (loop verbose unscoped)
loop :: Bool -> [Binding] -> InputT IO ()
loop verbose bindings = 
  do line <- getInputLine ">> "
     case line of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do let parsed = Read.read input
                         let imp = case parsed of
                                        Import s -> s
                                        otherwise -> []
                         let result = eval parsed bindings
                         if verbose then outputStrLn (show parsed)
                                    else return ()
                         let newBindings = case parsed of
                                             Def id x Placeholder -> [(id, ([], x))]
                                             EagerDef id x Placeholder -> [(id, ([], (case eval x bindings of
                                                                                        Result r -> Val r
                                                                                        Exception s -> Undefined s
                                                                                      )))]
                                             Defun id params x Placeholder -> [(id, (params, x))]
                                             otherwise -> []
                         case parsed of
                            Output x y -> outputStrLn (case (eval x bindings) of
                                                         Result (Str s) -> s
                                                         e -> show e)
                            otherwise -> outputStrLn (show result)
                         loop verbose (newBindings ++ bindings)
