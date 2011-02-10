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
import Control.Concurrent
import Data.List
import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.Console.Haskeline.Completion
import Scotch.Parse.Parse as Parse
import Scotch.Eval.ReadFile
import Scotch.Types.Types
import Scotch.Types.Exceptions
import Scotch.Types.Bindings
import Scotch.Types.Hash
import Scotch.Eval.Eval
import Scotch.Eval.Substitute


-- check for -v or -i flags
vFlag [] = False
vFlag (h:t) = if h == "-v" then True else vFlag t
iFlag [] = False
iFlag (h:t) = if h == "-i" then True else iFlag t
eFlag [] = False
eFlag (h:t) = if h == "-e" then True else eFlag t

nextQName [] = []
nextQName (h:t) = if h == '.' then t else nextQName t

nameMatch str [] = False
nameMatch str name = if isPrefixOf str name then True 
                     else nameMatch str (nextQName name)

main = do args <- getArgs          
          let verbose = vFlag args
          let interpret = iFlag args
          let evaluate = eFlag args
          -- import std.lib
          bindings <- importFile verbose ["std", "lib"] ["std", "lib"]
          let completionFunction str = do return $ [Completion { replacement = binding,
                                                                 display = binding,
                                                                 isFinished = False }
                                                    | i <- snd bindings, binding <- sort (nub [stripName (fst a) | a <- i]), nameMatch str binding]

          state <- initializeInput (setComplete (completeWord Nothing " " (completionFunction)) defaultSettings)
          bindings' <- case bindings of
                         (False, _) -> do putStrLn "Failed to import std.lib."
                                          return emptyHash
                         (True, b) -> do return $ b
          if verbose then putStrLn "-v Verbose mode on" else return ()
          if (length args) > 0 && not (isPrefixOf "-" (args !! 0))
            -- if a .sco filename is given as the first argument, interpret that file
            then if evaluate
                 then do statement <- wexecute verbose (Parse.read "" (args !! 0)) (snd bindings)
                         if interpret then loop verbose statement state
                                      else return ()
                 else do let filename = case isSuffixOf ".sco" (args !! 0) of
                                         True -> [(args !! 0) !! n| n <- [0..length (args !! 0) - 5]]
                                         False -> args !! 0
                         newbindings <- execute verbose filename bindings'
                         -- if the -i flag is set, start the interpreter
                         if interpret then loop verbose newbindings state
                                      else return ()
            -- otherwise, start the interpreter
            else do wexecute False [(Nothing, (Var (Name "startup")))] bindings'
                    loop verbose bindings' state

-- the interpreter's main REPL loop
loop :: Bool -> VarDict -> InputState -> IO ()
loop verbose [] state = loop verbose emptyHash state
loop verbose bindings state = 
  do line <- queryInput state (getInputLine ">> ")
     case line of
        Nothing -> return ()
        Just "quit" -> closeInput state
        Just "restart" -> main
        Just "vars" -> do putStrLn (foldl (++) "" 
                                   ["** " ++ show binding ++ "\n" | e <- bindings, binding <- e])
                          loop verbose bindings state
        Just "-v" -> loop (not verbose) bindings state
        Just input -> do -- parse input
                         let readinput = Parse.read "Interpreter" input
                         parsed <- case length readinput of
                                     0 -> do return (Skip)
                                     1 -> subfile (snd $ head readinput) bindings
                                     otherwise -> do return exEvalMultiple
                         -- evaluate parsed input
                         result <- do r <- ieval parsed bindings
                                      case r of
                                        Func f args -> return $ exNoMatch f args
                                        LambdaCall x args -> return $ exNoMatch x args
                                        otherwise -> return otherwise
                         imp' <- case parsed of
                                   Import s t -> importFile verbose s t
                                   otherwise -> case result of
                                                  Import s t -> importFile verbose s t 
                                                  otherwise -> do return (False, [])
                         imp <- case imp' of
                                  (False, []) -> do -- there was no attempted import
                                                   return emptyHash
                                  (False, f) -> do -- the imported module failed to open
                                                    putStrLn ("Failed to open " ++ show (parsed))
                                                    return emptyHash
                                  (True, b) -> do -- successful module import
                                                  return b
                         if verbose then putStrLn (show parsed)
                                    else return ()
                         -- determine whether any definitions were made
                         newBindings <- case parsed of
                                          Def id x Skip -> do return [(localId id, ([], x))]
                                          EagerDef id x Skip -> do evaluated <- ieval x bindings
                                                                   case evaluated of
                                                                     Exception e -> do putStrLn $ show $ Exception e
                                                                                       return []
                                                                     otherwise -> return [(localId id, ([], evaluated))]
                                          Defun id p x s -> do return $ newDefs $ Defun id p x s
                                          Defproc id params x Skip -> do return [(localId id, (params, Val (Proc x))), (id, ([], Val (HFunc id)))]
                                          otherwise -> case result of
                                                         Val (Proc p) -> do e <- (wexecute verbose 
                                                                                  [(Nothing, e) | e <- p] 
                                                                                  bindings)
                                                                            return $ [i | j <- e, i <- j]
                                                         otherwise -> do return []
                         -- output, if necessary
                         case result of
                           Output p -> case p of 
                                         Val (Str s) -> putStrLn s
                                         Val (Atom s l) -> do result <- ieval (Func (Name "show") [p]) bindings
                                                              case result of
                                                                Val (Str s) -> putStrLn s
                                                                Func f args -> putStrLn $ show $ exNoMatch f args
                                                                otherwise -> putStrLn $ show otherwise
                                         Func f args -> putStrLn $ show $ exNoMatch f args
                                         otherwise -> putStrLn (show p)
                           FileWrite (Val (File f)) (Val (Str x)) -> writeFile f x
                           FileAppend (Val (File f)) (Val (Str x)) -> appendFile f x
                           Val (Thread th) -> do forkIO (do wexecute verbose [(Nothing, th)] bindings
                                                            return ())
                                                 return ()
                           Val (Proc p) -> return ()
                           Val v -> case v of
                                      Atom s l -> do result <- ieval (Func (Name "show") [result]) bindings
                                                     case result of
                                                       Val (Str s) -> putStrLn s
                                                       otherwise -> putStrLn $ show otherwise
                                      otherwise -> putStrLn $ show $ result
                           Exception e -> putStrLn $ show $ Exception e 
                           otherwise -> return ()
                         -- continue loop
                         loop verbose (newBindingHash (newBindings ++ [i | j <- imp, i <- j]) bindings) state
