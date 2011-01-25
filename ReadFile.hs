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
import Data.ByteString.Lazy (readFile)
import System.Directory
import System.Environment.Executable
import Control.Concurrent
import Text.Parsec.Pos
import Parse
import Types
import Exceptions
import Bindings
import Eval
import Hash


-- interpret a list of code lines using a list of scoped bindings
wexecute :: Bool -> [PosExpr] -> VarDict -> IO VarDict
wexecute _ [] bindings = do return bindings
wexecute verbose (h:t) bindings = 
  do parsed <- subfile (snd h) bindings
     -- evaluate the parsed code
     result <- do r <- ieval parsed bindings
                  case r of
                    Func f args -> return $ exNoMatch f args
                    otherwise -> return otherwise
     if verbose then putStrLn (show parsed)
                else return ()        
     let newBindings = case parsed of
                         Def id x Skip -> do return [(id, ([], x))]
                         EagerDef id x Skip -> do return [(id, ([], eval x bindings))]
                         Defun id p x s -> do return $ newDefs $ Defun id p x s
                         Defproc id params x Skip -> do return [(id, (params, Val (Proc x))), (id, ([], Val (HFunc id)))]
                         Import s t -> do i <- importFile verbose s t
                                          b <- case i of 
                                                 (False, _) -> do putStrLn ("Failed to import module " ++ show s)
                                                                  return []
                                                 (True, i) -> do return [e | j <- i, e <- j]
                                          return b
                                        
                         otherwise -> case result of
                                        Val (Proc p) -> do e <- wexecute verbose [(position, e) | e <- p] bindings
                                                           return [i | j <- e, i <- j]
                                        otherwise -> do return []
     case result of
       Exception e -> do putStrLn ("\nException in " ++ (showPosition) ++ "\n" ++ e ++ "\n")
                         return []
       Output x -> do case x of
                        Val (Str s) -> putStrLn s
                        Val (Atom s l) -> putStrLn $ case eval (Func (Name "show") [x]) bindings of
                                                       Val (Str s) -> s
                                                       otherwise -> show otherwise
                        otherwise -> putStrLn (show x)
                      wexecute verbose t bindings
       FileWrite (Val (File f)) (Val (Str x)) -> do writeFile f x
                                                    wexecute verbose t bindings
       FileAppend (Val (File f)) (Val (Str x)) -> do appendFile f x
                                                     wexecute verbose t bindings
       Val (Thread th) -> do forkIO (do wexecute verbose [(Nothing, th)] bindings
                                        return ())
                             wexecute verbose t bindings
       otherwise -> do new <- newBindings
                       wexecute verbose t (addBindings new bindings)
     where name = case position of
                    Just p -> fst p
                    Nothing -> ""
           line = case position of
                    Just p -> fst (snd p)
                    Nothing -> 1
           column = case position of
                     Just p -> snd (snd p)
                     Nothing -> 1
           showPosition = name ++ ": Line " ++ show line ++ ", column " ++ show column
           position = fst h

-- returns a qualified file name from a list of identifiers provided by an import statement        
importName [] = ""
importName (h:t) = "/" ++ h ++ (importName t)
searchPathMatch :: [String] -> IO String
searchPathMatch [] = do return ""
searchPathMatch (h:t) = do exists <- doesFileExist (h ++ ".sco")
                           case exists of
                             True -> return h
                             False -> searchPathMatch t
-- returns (was the import successful?, VarDict of imported bindings)
importFile :: Bool -> [String] -> [String] -> IO (Bool, VarDict)
importFile verbose s t = 
  do currDir <- getCurrentDirectory
     fullPath <- splitExecutablePath
     let libDir = (fst fullPath) ++ "scotch.lib"
     let moduleName = importName s
     let searchPath = [currDir ++ moduleName ++ "/main",
                       currDir ++ moduleName,
                       libDir ++ moduleName ++ "/main",
                       libDir ++ moduleName]
     path <- searchPathMatch searchPath
     stdlib <- if s == ["std", "lib"] then do return (False, []) 
                                      else importFile False ["std", "lib"] ["std", "lib"]
     let builtin = case stdlib of
                    (True, b) -> b
                    (False, _) -> emptyHash
     val <- case path of 
              "" -> do return []
              otherwise -> do e <- execute verbose path builtin
                              return [i | j <- e, i <- j]
     let success = case path of
                     "" -> False
                     otherwise -> True
     let newval = [newbinding | newbinding <- 
                    [((case fst binding of
                         Name n -> Name (qualifier ++ n)), 
                       case snd binding of
                         ([], Val (HFunc (Name n))) -> if fst binding == Name n 
                                                       then ([], Val (HFunc (Name (qualifier ++ n))))
                                                       else snd binding
                         otherwise -> otherwise) | binding <- val],
                       (name newbinding) !! 0 /= '_' &&
                       (s == ["std", "lib"] ||
                        not (isInfixOf "." (snd $ splitAt (length qualifier) (name newbinding))))]
                   where qualifier = (foldl (++) [] [i ++ "." | i <- t])
                         name b = case fst b of 
                                    Name n -> n
     return (success, newBindingHash newval emptyHash)

-- interpret the contents of a file
execute :: Bool -> String -> VarDict -> IO VarDict
execute verbose file bindings = do optimized <- doesFileExist (file ++ ".osc")
                                   input <- Prelude.readFile (file ++ ".sco")
                                   parsed <- case optimized of
                                               True -> do bytes <- Data.ByteString.Lazy.readFile (file ++ ".osc")
                                                          return $ Parse.readBinary (bytes)
                                               False -> do let exprs = (Parse.read (file ++ ".sco") input)
                                                           serialize (file ++ ".osc") exprs
                                                           return exprs
                                   wexecute verbose parsed bindings
