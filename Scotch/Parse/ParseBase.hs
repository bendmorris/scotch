{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOnoR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Scotch.Parse.ParseBase where

import System.IO
import Control.Monad
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Prim
import Scotch.Types.Types
import qualified Text.Parsec.Token as Token


languageDef =
  emptyDef { Token.commentStart    = "",
             Token.commentEnd      = "",
             Token.commentLine     = "#",
             Token.identStart      = oneOf ("_" ++ lowerCase ++ upperCase),
             Token.identLetter     = oneOf (lowerCase ++ upperCase ++ numeric ++ idSymbol),
             Token.reservedNames   = ["if", "then", "else",
                                      "for", "in",
                                      "skip", "null",
                                      "true", "false",
                                      "and", "or", "not", "mod",
                                      "where", "case", "of",
                                      "do",
                                      "thread",
                                      "lambda",
                                      "import", "as",
                                      "take", "from",
                                      "using", "rule"
                                     ],
             Token.reservedOpNames = forbiddenOps
           }
           
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier       lexer -- an identifier
symbol     = Token.symbol           lexer -- a symbol
reserved   = Token.reserved         lexer -- a reserved name
reservedOp = Token.reservedOp       lexer -- an operator
parens     = Token.parens           lexer -- ( )
brackets   = Token.brackets         lexer -- [ ]
braces     = Token.braces           lexer -- { }
angles     = Token.angles           lexer -- < >
integer    = Token.integer          lexer -- an integer
float      = Token.float            lexer -- a float
whiteSpace = Token.whiteSpace       lexer -- whitespace
comment    = Token.commentLine      languageDef -- line comment
