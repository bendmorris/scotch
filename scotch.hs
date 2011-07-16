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
import System.IO
import Control.Concurrent
import Data.List
import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.Console.Haskeline.Completion
import Scotch.Config
import Scotch.Parse.Parse as Parse
import Scotch.Eval.ReadFile
import Scotch.Types.Types
import Scotch.Types.Exceptions
import Scotch.Types.Bindings
import Scotch.Types.Hash
import Scotch.Types.Interpreter
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
          let (verbose, interpret', strict, evaluate) = getFlags args (False, False, False, False)
          let interpret = if length args == 0 || take 1 (args !! 0) == "-" then True else interpret'
          -- import std.lib
          exePath <- getExecutablePath
          exeMod <- getModificationTime (exePath)
          importStdLib <- importFile (InterpreterSettings {verbose = verbose, interpret = False, strict = strict, exePath = exePath, exeMod = exeMod, stdlib = emptyHash}) 
                                     ["std", "lib"] ["std", "lib"]
          stdlib <- case importStdLib of
                      (False, _) -> do putStrLn "Failed to import std.lib."
                                       return emptyHash
                      (True, b) -> do return $ b
          let settings = InterpreterSettings {
                                              verbose = verbose,
                                              strict = strict,
                                              interpret = interpret,
                                              exePath = exePath,
                                              exeMod = exeMod,
                                              stdlib = stdlib
                                              }

          state <- initializeInput (setComplete (completeWord Nothing " " (completionFunction stdlib)) defaultSettings)
          if verbose then putStrLn "-v Verbose mode on" else return ()
          if (length args) > 0 && not (isPrefixOf "-" (args !! 0))
            -- if a .sco filename is given as the first argument, interpret that file
            then if evaluate
                 then do statement <- wexecute settings
                                               (Parse.read "" (args !! 0)) 
                                               stdlib
                         if interpret then loop settings statement state
                                      else return ()
                 else do let filename = case isSuffixOf ".sco" (args !! 0) of
                                         True -> [(args !! 0) !! n| n <- [0..length (args !! 0) - 5]]
                                         False -> args !! 0
                         newbindings <- execute settings filename stdlib
                         -- if the -i flag is set, start the interpreter
                         if interpret then loop settings newbindings state
                                      else return ()
            -- otherwise, start the interpreter
            else do putStrLn $ "Scotch, version " ++ version ++ " on " ++ environment
                    putStrLn "For more information, type \"about\", \"copyright\" or \"license\"."
                    loop settings stdlib state


-- the interpreter's main REPL loop
loop :: InterpreterSettings -> VarDict -> InputState -> IO ()
loop settings [] state = loop settings emptyHash state
loop settings bindings state = 
  do line <- queryInput state (getInputLine ">> ")
     case line of
        Nothing -> return ()
        Just "quit" -> closeInput state
        Just "restart" -> main
        Just "vars" -> do putStrLn $ foldl (++) "" 
                                     ["** " ++ show (fst binding) ++ " = " ++ show (snd binding) ++ "\n" 
                                      | e <- bindings, binding <- e]
                          loop settings bindings state
        Just "-v" -> loop (InterpreterSettings {verbose = not (verbose settings),
                                                strict = strict settings,
                                                interpret = interpret settings,
                                                exePath = exePath settings,
                                                exeMod = exeMod settings,
                                                stdlib = stdlib settings
                                                }) bindings state
        Just "-s" -> loop (InterpreterSettings {verbose = verbose settings,
                                                strict = not (strict settings),
                                                interpret = interpret settings,
                                                exePath = exePath settings,
                                                exeMod = exeMod settings,
                                                stdlib = stdlib settings
                                                }) bindings state
        Just input -> do -- parse input
                         let parsed = Parse.read "Interpreter" input
                         newBindings <- case length parsed of
                                          0 -> do return []
                                          1 -> wexecute settings parsed bindings
                                          otherwise -> do putStrLn (show $ exEvalMultiple (Just $ parsed !! 0) (Just $ parsed !! 1))
                                                          return []
                         let newVars = (makeVarDict (reverse [i | j <- newBindings, i <- j]) bindings)
                         --closeInput state
                         --state' <- initializeInput (setComplete (completeWord Nothing " " (completionFunction newVars)) defaultSettings)
                         loop settings
                              newVars
                              state --state'
                              
                              
completionFunction :: VarDict -> String -> IO [Completion]
completionFunction vars str = do return $ [Completion { replacement = str' ++ "." ++ binding,
                                                        display = str' ++ "." ++ binding,
                                                        isFinished = False }
                                          | binding <- sort (nub [fst i | i <- qualVarHash str' vars])]
                                 where str' = if last str == '.' then take ((length str) - 1) str else str
