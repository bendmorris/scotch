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

-- determines the amount of whitespace in a line (includes comments)
whitespace :: String -> Int
whitespace [] = 0
whitespace (h:t) = case h of 
                     ' ' -> 1 + (whitespace t) 
                     '\t' -> 4 + (whitespace t)
                     '#' -> 1 + (whitespace [' ' | c <- t])
                     otherwise -> 0

-- removes all bindings that are no longer relevant at the current scope
scoped_bindings :: Int -> [ScopedBinding] -> [ScopedBinding]
scoped_bindings _ [] = []
scoped_bindings scope (h:t) = if scope < (fst h) then scoped_bindings scope t
                                                 else h : (scoped_bindings scope t)

-- interpret a list of code lines using a list of scoped bindings
wexecute :: Bool -> [String] -> [ScopedBinding] -> Int -> IO [ScopedBinding]
wexecute _ [] bindings _ = do return bindings
wexecute verbose (h:t) bindings line = 
  do -- ensure that the line is not entirely whitespace or comments
     if (length input) - scope > 0 then
         do if verbose then putStrLn (show parsed)
                       else return ()
            case parsed of
               -- output if the parsed code results in output
               Output x y -> output x               
               otherwise -> case result of
                                Exception e -> do putStrLn ("Exception on line " ++ (show line) ++ "\n" ++ e)
                                                  return []
                                otherwise -> case parsed of
                                                -- output if the evaluated code results in output
                                                Output x y -> do output x
                                                                 return []
                                                otherwise -> do new <- newBindings
                                                                wexecute verbose t (new ++ bindings') (line+1)
       -- if the line consists of all whitespace, continue to the next line
       else wexecute verbose t (bindings) (line+1)
     where input = h
           -- scope is determined by amount of leading whitespace
           scope = whitespace input
           -- parse the code
           parsed = Read.read input
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
                            Defun id params x Placeholder -> do return [(scope, (id, (params, x)))]
                            Import s -> do i <- importFile verbose scope s
                                           b <- case i of 
                                                  (False, _) -> do putStrLn ("Failed to import module " ++ show s)
                                                                   return []
                                                  (True, i) -> do return i
                                           return b
                                           
                            otherwise -> do return []
           -- interpret another file and add its final definitions to the stack            
            
           output x = case (eval x unscoped) of
                        Exception e -> do putStrLn ("Exception on line " ++ (show line) ++ ":\n\t" ++ e)
                                          return []
                        Result (Str s) -> do putStrLn s
                                             new <- newBindings
                                             val <- wexecute verbose t (new ++ bindings') (line+1)
                                             return val
                        s -> do putStrLn (show s)
                                new <- newBindings
                                val <- wexecute verbose t (new ++ bindings') (line+1)
                                return val

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

-- returns a qualified file name from a list of identifiers provided by an import statement        
importFileName [] = ".sco"
importFileName (h:t) = "/" ++ h ++ (importFileName t)
-- returns (was the import successful?, list of imported bindings)
importFile verbose scope s = do currDir <- getCurrentDirectory
                                full_path <- splitExecutablePath
                                let exepath = (fst full_path) ++ "scotch.lib"
                                currDir_exists <- doesFileExist (currDir ++ importFileName s)
                                exepath_exists <- doesFileExist (exepath ++ importFileName s)
                                -- search current directory, then executable directory
                                let path = case (s !! 0) of
                                             "std" -> exepath ++ (importFileName s)                                             
                                             otherwise -> case (currDir_exists, exepath_exists) of
                                                            (True, _) -> currDir ++ importFileName s
                                                            (False, True) -> exepath ++ importFileName s
                                                            (False, False) -> ""
                                val <- case path of 
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
                                   wexecute verbose 
                                            (split '\n' (replace input "\\\n" "") ) 
                                            ([(0, binding) | binding <- bindings]) 
                                            1
