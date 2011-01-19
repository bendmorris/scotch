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

module Parse where

import Text.ParserCombinators.Parsec
import Types
import Hash
import Expressions
import ParseBase

parser :: Parser [PosExpr]
parser = many (whiteSpace >> statement)

summary [] = []
summary (h:t) = if h == '\n' then "" else h : summary t

statement :: Parser PosExpr
statement = try (do pos <- getPosition
                    let col = sourceColumn pos
                    expr <- expression col
                    return (Just pos, expr))
            <|> (do pos <- getPosition
                    chars <- many1 (noneOf "")
                    return (Just pos, Exception $ "Parse error: Unable to parse text starting with \"" ++ summary (take 30 chars) ++ "\""))
                           

read name s = case (parse parser name s) of
                Right r -> r
                otherwise -> [(Nothing, Exception "Parse error")]
