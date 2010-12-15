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
  
whitespace :: String -> Int
whitespace [] = 0
whitespace (h:t) = case h of 
                     ' ' -> 1 + (whitespace t) 
                     '\t' -> 4 + (whitespace t)
                     '#' -> 1 + (whitespace [' ' | c <- t])
                     otherwise -> 0

scoped_bindings :: Int -> [ScopedBinding] -> [ScopedBinding]
scoped_bindings _ [] = []
scoped_bindings scope (h:t) = if scope < (fst h) then scoped_bindings scope t
                                                 else h : (scoped_bindings scope t)
peeled_bindings :: [ScopedBinding] -> [Binding]
peeled_bindings [] = []
peeled_bindings (h:t) = snd h : peeled_bindings t

wexecute :: Bool -> [String] -> [ScopedBinding] -> Int -> IO [ScopedBinding]
wexecute _ [] bindings _ = do return bindings
wexecute verbose (h:t) bindings line = 
  do -- putStrLn (show bindings)
     if (length input) - scope > 0 then 
         do if verbose then putStrLn (show parsed)
                       else return ()
            case parsed of
               Output x y -> output x               
               otherwise -> case result of
                                Exception e -> do putStrLn ("Exception on line " ++ (show line) ++ "\n" ++ e)
                                                  return []
                                otherwise -> case parsed of
                                                Output x y -> do output x
                                                                 return []
                                                otherwise -> do new <- newBindings
                                                                wexecute verbose t (new ++ bindings') (line+1)
            
      else wexecute verbose t (bindings) (line+1)
      where input = h
            scope = whitespace input
            parsed = Read.read input
            bindings' = scoped_bindings scope bindings
            peeled = peeled_bindings bindings'
            result = eval parsed peeled
            newBindings :: IO ([ScopedBinding])
            newBindings = case parsed of
                             Def id x Placeholder -> do return [(scope, (id, ([], x)))]
                             EagerDef id x Placeholder -> do return [(scope, (id, ([], (case eval x peeled of
                                                                                          Result r -> Val r
                                                                                          Exception s -> Undefined s
                                                                                        ))))]
                             Defun id params x Placeholder -> do return [(scope, (id, (params, x)))]
                             Import s -> importFile s
                             otherwise -> do return []
            importFileName [] = ".sco"
            importFileName (h:t) = "/" ++ h ++ (importFileName t)            
            importFile s = do currDir <- getCurrentDirectory
                              full_path <- splitExecutablePath
                              let exepath = (fst full_path)
                              let path = case (s !! 0) of
                                           "std" -> exepath ++ "scotch." ++ tail (importFileName s)
                                           otherwise -> currDir ++ importFileName s
                              val <- execute verbose (path) []
                              let newval = [(scope, snd binding) | binding <- val]
                              return newval
            
            output x = case (eval x peeled) of
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

execute :: Bool -> String -> [Binding] -> IO [ScopedBinding]
execute verbose file bindings = do input <- readFile file
                                   wexecute verbose 
                                            (split '\n' (replace input "\\\n" "") ) 
                                            ([(0, binding) | binding <- bindings]) 
                                            1
