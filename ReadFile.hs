module ReadFile where

import Data.List
import System.Directory
import System.Environment.Executable
import Text.ParserCombinators.Parsec
import Read
import Types
import Eval

split :: Char -> String -> [String]
split = unfoldr . split'
split' :: Char -> String -> Maybe (String, String)
split' c l
  | null l = Nothing
  | otherwise = Just (h, drop 1 t)
  where (h, t) = span (/=c) l

-- removes all bindings that are no longer relevant at the current scope
scoped_bindings :: Int -> [ScopedBinding] -> [ScopedBinding]
scoped_bindings _ [] = []
scoped_bindings scope (h:t) = if scope < (fst h) then scoped_bindings scope t
                                                 else h : (scoped_bindings scope t)

-- interpret a list of code lines using a list of scoped bindings
wexecute :: Bool -> [PosExpr] -> [ScopedBinding] -> IO [ScopedBinding]
wexecute _ [] bindings = do return bindings
wexecute verbose (h:t) bindings = 
  do -- ensure that the line is not entirely whitespace or comments
     do if verbose then putStrLn (show parsed)
                   else return ()
        case parsed of
           -- output if the parsed code results in output
           Output x y -> output x               
           otherwise -> case result of
                            Exception e -> do putStrLn ("\nException in " ++ (showPosition) ++ "\n" ++ e ++ "\n")
                                              return []
                            otherwise -> case parsed of
                                            -- output if the evaluated code results in output
                                            Output x y -> do output x
                                                             return []
                                            otherwise -> do new <- newBindings
                                                            wexecute verbose t (new ++ bindings')
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
           parsed = snd h
           -- remove any bindings no longer relevant at current scope
           bindings' = scoped_bindings scope bindings
           unscoped = unscope bindings'
           -- evaluate the parsed code
           result = eval parsed unscoped
           -- determine whether any new definitions were made, and apply them at this scope
           newBindings :: IO ([ScopedBinding])
           newBindings = case parsed of
                            Def id x Placeholder -> do return [(scope, (id, ([], x)))]
                            EagerDef id x Placeholder -> do return [(scope, (id, ([], (case eval x unscoped of
                                                                                         Result r -> Val r
                                                                                         Exception s -> Undefined s
                                                                                       ))))]
                            Defun id params x Placeholder -> do return [(scope, (id, (params, x))), (scope, (id, ([], Val (HFunc id))))]
                            Defproc id params x Placeholder -> do return [(scope, (id, (params, Val (Proc x)))), (scope, (id, ([], Val (HFunc id))))]
                            Import s -> do i <- importFile verbose scope s
                                           b <- case i of 
                                                  (False, _) -> do putStrLn ("Failed to import module " ++ show s)
                                                                   return []
                                                  (True, i) -> do return i
                                           return b
                                           
                            otherwise -> case result of
                                           Result (Proc p) -> wexecute verbose [(position, e) | e <- p] bindings
                                           otherwise -> do return []            
           output x = case (eval x unscoped) of
                        Exception e -> do putStrLn ("Exception on line " ++ (show line) ++ ":\n\t" ++ e)
                                          return []
                        Result (Str s) -> do putStrLn s
                                             new <- newBindings
                                             val <- wexecute verbose t (new ++ bindings')
                                             return val
                        s -> do putStrLn (show s)
                                new <- newBindings
                                val <- wexecute verbose t (new ++ bindings')
                                return val

-- returns a qualified file name from a list of identifiers provided by an import statement        
importFileName s = importName s ++ ".sco"
importName [] = ""
importName (h:t) = "/" ++ h ++ (importName t)
-- returns (was the import successful?, list of imported bindings)
importFile verbose scope s = do currDir <- getCurrentDirectory
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
                                if dir then importFile verbose scope (s ++ ["main"])
                                 else do val <- case path of 
                                                  "" -> do return []
                                                  otherwise -> execute verbose (path) []
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
