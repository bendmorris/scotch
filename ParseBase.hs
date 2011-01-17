module ParseBase where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lowerCase = "abcdefghijklmnopqrstuvwxyz"
numeric = "0123456789"
idSymbol = "_!'."
operatorSymbol = "!@#$%^&*+-*/=<>?|`:"
forbiddenOps = ["=", ":=", ":"]

languageDef =
  emptyDef { Token.commentStart    = "{-",
             Token.commentEnd      = "-}",
             Token.commentLine     = "#",
             Token.identStart      = oneOf ("_" ++ lowerCase),
             Token.identLetter     = oneOf (lowerCase ++ upperCase ++ numeric ++ idSymbol),
             Token.reservedNames   = ["if", "then", "else",
                                      "for", "in",
                                      "print", "skip",
                                      "true", "false",
                                      "and", "or", "not",
                                      "where", "case", "of",
                                      "do",
                                      "int", "float", "str",
                                      "read", "write", "append", "input",
                                      "thread",
                                      "lambda",
                                      "import", "as",
                                      "take", "from",
                                      ","
                                     ],
             Token.reservedOpNames = ["+", "-", "*", "/", "^", "=", ":=", "==",
                                      "!=", "<", ">", "and", "or", "not", ":", "->",
                                      "<=", ">=", "+=", "<<", ">>", "..", "::",
                                      "@", "mod", "%"
                                     ]
           }
           
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier       lexer -- parses an identifier
symbol     = Token.symbol           lexer -- parses a symbol
reserved   = Token.reserved         lexer -- parses a reserved name
reservedOp = Token.reservedOp       lexer -- parses an operator
parens     = Token.parens           lexer -- parses [ ]
brackets   = Token.brackets         lexer -- parses [ ]
braces     = Token.braces           lexer -- parses { }
angles     = Token.angles           lexer -- parses < >
integer    = Token.integer          lexer -- parses an integer
float      = Token.float            lexer -- parses a float
whiteSpace = Token.whiteSpace       lexer -- parses whitespace
