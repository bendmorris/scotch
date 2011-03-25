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


-- check for flags
getFlags (h:t) (v, i, s, e) = case h of
                                "-v" ->             getFlags t (True, i, s, e)
                                "--verbose" ->      getFlags t (True, i, s, e)
                                "-i" ->             getFlags t (v, True, s, e)
                                "--interpret" ->    getFlags t (v, True, s, e)
                                "-s" ->             getFlags t (v, i, True, e)
                                "--strict" ->       getFlags t (v, i, True, e)
                                "-e" ->             getFlags t (v, i, s, True)
                                "--eval" ->         getFlags t (v, i, s, True)
                                otherwise -> getFlags t (v, i, s, e)
getFlags [] a = a

nextQName [] = []
nextQName (h:t) = if h == '.' then t else nextQName t

modNameMatch str [] = False
modNameMatch str name = if isPrefixOf str name then True 
                        else modNameMatch str (nextQName name)

main = do args <- getArgs          
          let (verbose, interpret, strict, evaluate) = getFlags args (False, False, False, False)
          -- import std.lib
          bindings <- importFile (verbose, strict) ["std", "lib"] ["std", "lib"]
          let completionFunction str = do return $ [Completion { replacement = binding,
                                                                 display = binding,
                                                                 isFinished = False }
                                                    | i <- snd bindings, binding <- sort (nub [show (fst a) | a <- i]), modNameMatch str binding]

          state <- initializeInput (setComplete (completeWord Nothing " " (completionFunction)) defaultSettings)
          bindings' <- case bindings of
                         (False, _) -> do putStrLn "Failed to import std.lib."
                                          return emptyHash
                         (True, b) -> do return $ b
          if verbose then putStrLn "-v Verbose mode on" else return ()
          if (length args) > 0 && not (isPrefixOf "-" (args !! 0))
            -- if a .sco filename is given as the first argument, interpret that file
            then if evaluate
                 then do statement <- wexecute (verbose, True, strict) 
                                               (Parse.read "" (args !! 0)) 
                                               (snd bindings)
                         if interpret then loop (verbose, strict) statement state
                                      else return ()
                 else do let filename = case isSuffixOf ".sco" (args !! 0) of
                                         True -> [(args !! 0) !! n| n <- [0..length (args !! 0) - 5]]
                                         False -> args !! 0
                         newbindings <- execute (verbose, strict) filename bindings'
                         -- if the -i flag is set, start the interpreter
                         if interpret then loop (verbose, strict) newbindings state
                                      else return ()
            -- otherwise, start the interpreter
            else do wexecute (False, False, True) [(Nothing, (Var "startup"))] bindings'
                    loop (verbose, strict) bindings' state

-- the interpreter's main REPL loop
loop :: (Bool, Bool) -> VarDict -> InputState -> IO ()
loop (verbose, strict) [] state = loop (verbose, strict) emptyHash state
loop (verbose, strict) bindings state = 
  do line <- queryInput state (getInputLine ">> ")
     case line of
        Nothing -> return ()
        Just "quit" -> closeInput state
        Just "restart" -> main
        Just "vars" -> do putStrLn $ show bindings--(foldl (++) "" 
                                   --["** " ++ show binding ++ "\n" | e <- bindings, binding <- e])
                          loop (verbose, strict) bindings state
        Just "-v" -> loop (not verbose, strict) bindings state
        Just "-s" -> loop (verbose, not strict) bindings state
        Just input -> do -- parse input
                         let parsed = Parse.read "Interpreter" input
                         newBindings <- case length parsed of
                                          0 -> do return []
                                          1 -> wexecute (verbose, True, strict) parsed bindings
                                          otherwise -> do putStrLn (show exEvalMultiple)
                                                          return []
                         loop (verbose, strict) 
                              (makeVarDict' [i | j <- newBindings, i <- j] bindings) 
                              state
