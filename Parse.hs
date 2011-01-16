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
parser = many statement

statement :: Parser PosExpr
statement = whiteSpace >> do pos <- getPosition
                             expr <- expression
                             return (Just pos, expr)

read name s = case (parse parser name s) of
                Right r -> r
                otherwise -> [(Nothing, Exception "Parse error")]
