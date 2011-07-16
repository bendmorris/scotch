{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in ther hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Scotch.Parse.Parse where

import Data.ByteString.Lazy
import Data.Binary
import Data.List
import Data.List.Utils
import Data.List.Split
import Codec.Compression.GZip
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos
import Scotch.Types.Types
import Scotch.Types.Binary
import Scotch.Types.Show
import Scotch.Types.Hash
import Scotch.Types.Exceptions
import Scotch.Parse.Expressions
import Scotch.Parse.ParseBase

parser n = 
  many (
  try (do whiteSpace
          pos <- getPosition
          expr <- statement
          return (Just (sourceName pos, ((sourceLine pos) + n - 1, sourceColumn pos)), expr))
  <|> 
  try (do whiteSpace
          pos <- getPosition
          chars <- many1 (noneOf "")
          return (Just (sourceName pos, ((sourceLine pos) + n - 1, sourceColumn pos)), 
                  Exception $ "Parse error: Unable to parse text starting with \"" ++ summary (Prelude.take 40 chars) ++ "\"")))

summary [] = []
summary (h:t) = if h == '\n' then "" else h : summary t

splitLines :: [String] -> [String] -> [(Int, String)]
splitLines [] a = Data.List.zip [1..] a
splitLines (h:[]) a = Data.List.zip [1..] (a ++ [h])
splitLines (h:t) a = if Prelude.length (Prelude.head t) > 0 && 
                        Prelude.head (Prelude.head t) == ' '
                     then splitLines ((h ++ "\n" ++ Prelude.head t) : Prelude.tail t) a
                     else splitLines t (a ++ [h])
leadons = [('(', ')'), ('{', '}'), ('\"', '\"')]
connectLines _ [] a = a
connectLines _ (h:[]) a = a ++ [h]
connectLines [] (h:t) a = connectLines leadons t (a ++ [h])
connectLines (l:m) (h:t) a = if (fst l == snd l && mod ((countElem (fst l) (snd h)) - (countSeq ['\\', fst l] (snd h) 0)) 2 /= 0) || 
                                (fst l /= snd l && countElem (fst l) (snd h) > countElem (snd l) (snd h))
                             then connectLines leadons ((fst h, snd h ++ "\n" ++ snd (Prelude.head t)) : 
                                                        Prelude.tail t) a
                             else connectLines m (h:t) a

countSeq seq [] a = a
countSeq seq (h:t) a = if Data.List.isPrefixOf seq (h:t) then countSeq seq t (a + 1) else countSeq seq t a
                           
read name text = [result l
                  | l <- realLines text,
                    snd (result l) /= Skip]
                 where result l = case parse (parser $ fst l) name (snd l) of
                                    Right r -> case Prelude.length r of
                                                 0 -> (Nothing, Skip)
                                                 1 -> r !! 0
                                                 otherwise -> (fst (r !! 1), exEvalMultiple (snd $ r !! 0) (snd $ r !! 1))
                                    Left l -> (Nothing, Exception $ "Parse error: " ++ show otherwise)
                                    
realLines text = connectLines leadons (splitLines (splitOn "\n" (replace "\\\n" "" text)) []) []
                  
                
serialize file exprs = Data.ByteString.Lazy.writeFile file (compress (encode (exprs :: [PosExpr])))
readBinary bytes = decode (decompress bytes) :: [PosExpr]
