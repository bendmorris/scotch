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

module Scotch.Types.Bindings where

import Data.List
import Scotch.Eval.Calc
import Scotch.Types.Types
import Scotch.Types.Hash
import Scotch.Types.Exceptions

-- binds a left term to a right term
type Binding = (Expr, Expr)
type VarDict = [[Binding]]

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

varName' :: String -> Int -> String
varName' s 0 = s
varName' s n = if s !! n == '.'
               then snd $ splitAt (n+1) s
               else varName' s (n-1)
varName "" = ""
varName s = varName' s ((length s) - 1)

exprHash :: Expr -> Int
exprHash (Add _ _) = 1
exprHash (Val (List _)) = 1
exprHash (ListExpr _) = 1
exprHash (Sub _ _) = 2
exprHash (Prod _ _) = 3
exprHash (Div _ _) = 4
exprHash (Exp _ _) = 5
exprHash (Mod _ _) = 6
exprHash (Eq _ _) = 7
exprHash (InEq _ _) = 7
exprHash (Gt _ _) = 8
exprHash (Lt _ _) = 9
exprHash (And _ _) = 10
exprHash (Or _ _) = 11
exprHash (Not _) = 12
exprHash (Subs _ _) = 13
exprHash (Take _ _) = 14
exprHash (Var v _) = hashLoc $ last (split v '.')
exprHash _ = 0

localId id = ("local." ++ stripLocal id)
stripLocal s = if isPrefixOf "local." s then [s !! n | n <- [length "local." .. (length s) - 1]] else s

emptyVarDict = [[] | n <- [1..hashSize]]

makeVarDict' :: [(Expr, Expr)] -> [[(Expr, Expr)]] -> [[(Expr, Expr)]]
makeVarDict' [] r = r
makeVarDict' (h:t) r = makeVarDict' t 
                        [(if exprHash (fst h) == i
                          then [h]
                          else [])
                         ++ (removeFromBucket h (r !! i))
                         | i <- [0..(hashSize-1)]]
makeVarDict :: [(Expr, Expr)] -> [[(Expr, Expr)]]
makeVarDict s = makeVarDict' s emptyVarDict
