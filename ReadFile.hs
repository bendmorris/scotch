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

module ReadFile where

import Data.List
import System.Directory
import System.Environment.Executable
import Text.ParserCombinators.Parsec
import Read
import Types
import Bindings
import Eval


-- interpret a list of code lines using a list of scoped bindings
wexecute :: Bool -> [PosExpr] -> [ScopedBinding] -> IO [ScopedBinding]
wexecute _ [] bindings = do return bindings
wexecute verbose (h:t) bindings = 
  do parsed <- subfile (snd h) unscoped
     -- evaluate the parsed code
     let result = eval parsed unscoped
     if verbose then putStrLn (show parsed)
                else return ()        
     let newBindings = case parsed of
                         Def id x Skip -> do return [(scope, (id, ([], x)))]
                         EagerDef id x Skip -> do return [(scope, (id, ([], eval x unscoped)))]
                         Defun id params x Skip -> do return [(scope, (id, (params, x))), (scope, (id, ([], Val (HFunc id))))]
                         Defproc id params x Skip -> do return [(scope, (id, (params, Val (Proc x)))), (scope, (id, ([], Val (HFunc id))))]
                         Import s -> do i <- importFile verbose scope s
                                        b <- case i of 
                                               (False, _) -> do putStrLn ("Failed to import module " ++ show s)
                                                                return []
                                               (True, i) -> do return i
                                        return b
                                        
                         otherwise -> case result of
                                        Val (Proc p) -> wexecute verbose [(position, e) | e <- p] bindings
                                        otherwise -> do return []            
     case result of
       Exception e -> do putStrLn ("\nException in " ++ (showPosition) ++ "\n" ++ e ++ "\n")
                         return []
       Output x -> do case x of
                        Val (Str s) -> putStrLn s
                        otherwise -> putStrLn (show x)
                      wexecute verbose t bindings'
       FileWrite (Val (File f)) (Val (Str x)) -> do writeFile f x
                                                    wexecute verbose t bindings'
       FileAppend (Val (File f)) (Val (Str x)) -> do appendFile f x
                                                     wexecute verbose t bindings'
       otherwise -> do new <- newBindings
                       wexecute verbose t (addBindings new bindings')
     where -- scope is determined by amount of leading whitespace
           scope = column
           name = case position of
                    Just p -> sourceName p
                    Nothing -> ""
           line = case position of
                    Just p -> sourceLine p
                    Nothing -> 1
           column = case position of
                     Just p -> sourceColumn p
                     Nothing -> 1
           -- parse the code
           showPosition = name ++ ": Line " ++ show line ++ ", column " ++ show column
           position = fst h           
           -- remove any bindings no longer relevant at current scope
           bindings' = scoped_bindings scope bindings
           unscoped = unscope bindings'

-- returns a qualified file name from a list of identifiers provided by an import statement        
importFileName s = importName s ++ ".sco"
importName [] = ""
importName (h:t) = "/" ++ h ++ (importName t)
-- returns (was the import successful?, list of imported bindings)
importFile verbose scope s = 
  do currDir <- getCurrentDirectory
     full_path <- splitExecutablePath
     let libpath = (fst full_path) ++ "scotch.lib"
     let ifn = importFileName s
     let idn = importName s
     currDir_file <- doesFileExist (currDir ++ ifn)
     currDir_dir <- doesDirectoryExist (currDir ++ idn)
     exeDir_file <- doesFileExist (libpath ++ ifn)
     exeDir_dir <- doesDirectoryExist (libpath ++ idn)
     let path | currDir_file = currDir ++ ifn
              | currDir_dir = currDir ++ idn
              | exeDir_file = libpath ++ ifn
              | exeDir_dir = libpath ++ idn
              | otherwise = ""
     let dir = currDir_dir || exeDir_dir
     stdlib <- if s == ["std", "lib"] then do return (False, []) 
                                      else importFile verbose 1 ["std", "lib"]
     let builtin = case stdlib of
                    (True, b) -> unscope b
                    (False, _) -> []
     if dir then importFile verbose scope (s ++ ["main"])
      else do val <- case path of 
                       "" -> do return []
                       otherwise -> execute verbose path builtin
              let success = case path of
                              "" -> False
                              otherwise -> True
              let newval = [(scope, snd binding) | binding <- val]
              return (success, newval)

-- interpret the contents of a file
execute :: Bool -> String -> [Binding] -> IO [ScopedBinding]
execute verbose file bindings = do input <- readFile file
                                   let parsed = Read.read file input
                                   wexecute verbose 
                                            parsed 
                                            ([(1, binding) | binding <- bindings])
