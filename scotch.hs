module Main where

import System
import Data.List
import Types
import Read
import Eval
import System.Console.Haskeline
import ReadFile


version = "0.1"
vFlag [] = False
vFlag (h:t) = if h == "-v" then True else vFlag t

unscope :: [ScopedBinding] -> [Binding]
unscope [] = []
unscope (h:t) = (snd h) : unscope t

main = do args <- getArgs
          bindings <- execute False "std/lib.sco" []
          let unscoped = unscope bindings
          let verbose = vFlag args
          if verbose then putStrLn "-v Verbose mode on" else return ()
          if (length args) > 0 && isSuffixOf ".sco" (args !! 0) 
            then do execute verbose (args !! 0) unscoped
                    return ()
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
                                             Def id x _ -> [(id, ([], x))]
                                             EagerDef id x _ -> [(id, ([], (case eval x bindings of
                                                                        Result r -> Val r
                                                                        Exception s -> Undefined s
                                                                       )))]
                                             Defun id params x _ -> [(id, (params, x))]
                                             otherwise -> []
                         --execute imp bindings
                         case parsed of
                            Output x y -> outputStrLn (show (eval x bindings))
                            otherwise -> outputStrLn (show result)
                         loop verbose (newBindings ++ bindings)
