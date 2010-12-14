module ReadFile where

import Text.ParserCombinators.Parsec
import Stdlib
import Read
import Types
import Eval
import Data.List

type ScopedBinding = (Int, Binding)

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
                     otherwise -> 0

scoped_bindings :: Int -> [ScopedBinding] -> [ScopedBinding]
scoped_bindings _ [] = []
scoped_bindings scope (h:t) = if scope < (fst h) then scoped_bindings scope t
                                                 else h : (scoped_bindings scope t)
peeled_bindings :: [ScopedBinding] -> [Binding]
peeled_bindings [] = []
peeled_bindings (h:t) = snd h : peeled_bindings t

wexecute :: [String] -> [ScopedBinding] -> Int -> IO ()
wexecute [] bindings _ = do return ()
wexecute (h:t) bindings line = 
  do if (length input) - scope > 0 then 
         do 
            case parsed of
               Output x y -> output x
               otherwise -> case result of
                                Exception e -> putStrLn ("Exception on line " ++ (show line) ++ "\n" ++ e)
                                otherwise -> do case parsed of
                                                    Output x y -> output x
                                                    otherwise -> wexecute t (newBindings ++ bindings') (line+1)
            
      else wexecute t (bindings) (line+1)
      where input = h
            scope = whitespace input
            parsed = Read.read input
            bindings' = scoped_bindings scope bindings
            peeled = peeled_bindings bindings'
            result = eval parsed peeled
            newBindings = case parsed of
                             Def id x _ -> [(scope, (id, ([], x)))]
                             EagerDef id x _ -> [(scope, (id, ([], (case eval x peeled of
                                                                      Result r -> Val r
                                                                      Exception s -> Undefined s
                                                                    ))))]
                             Defun id params x _ -> [(scope, (id, (params, x)))]
                             otherwise -> []            
            output x = case (eval x peeled) of
                         Exception e -> putStrLn ("Exception on line " ++ (show line) ++ ":\n\t" ++ e)
                         s -> do putStrLn (show s)
                                 wexecute t (newBindings ++ bindings') (line+1)     

execute :: String -> [Binding] -> IO ()
execute file bindings = do input <- readFile file
                           wexecute (split '\n' input) ([(0, binding) | binding <- (bindings ++ stdlib)]) 1
