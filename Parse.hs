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

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Types
import Hash

upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lowerCase = "abcdefghijklmnopqrstuvwxyz"
numeric = "0123456789"
idSymbol = "_!'."

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
                                      "import", "as"
                                     ],
             Token.reservedOpNames = ["+", "-", "*", "/", "^", "=", ":=", "==",
                                      "!=", "<", ">", "and", "or", "not", ":", "->",
                                      "<=", ">=", "+=", "<<", ">>", "..", "::"
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

parser :: Parser [PosExpr]
parser = many statement

statement :: Parser PosExpr
statement = whiteSpace >> do pos <- getPosition
                             expr <- expression
                             return (Just pos, expr)

-- expression parsers

expression :: Parser Expr
expression = operation <|> term
           
operation = buildExpressionParser operators (term <|> parens term)

term :: Parser Expr
term = try syntax <|>
       parens expression

reservedWord = try (reserved "true" >> return (Bit True)) <|>
               try (reserved "false" >> return (Bit False)) <|>
               try (reserved "null" >> return (Null))
reservedExpr =
  do word <- reservedWord
     return $ Val word

value = 
  try reservedWord <|>
  try atomValue <|>
  try hashValue <|>
  try listValue <|> 
  try strValue <|> 
  try floatValue <|> 
  try intValue
valueStmt =
  try reservedExpr <|>
  try atomStmt <|>
  try procStmt <|>
  try hashStmt <|>
  try listStmt <|>
  try fileStmt <|>
  try lambdaStmt <|>
  try strStmt <|>
  try floatStmt <|>
  try intStmt
valueExpr = 
  try ifStmt <|>
  try caseStmt <|>
  try skipStmt <|>
  try readStmt <|>
  try inputStmt <|>
  try threadStmt <|>
  try rangeStmt <|>
  try forStmt <|>
  try notStmt <|>
  try conversionStmt <|>
  try valueStmt <|>
  try funcallStmt <|>
  try varcallStmt
     
syntax :: Parser Expr
syntax = 
  try printStmt <|>
  try importStmt <|>
  try assignment <|>
  try writeStmt <|>
  try appendStmt <|>
  try whereStmt <|>
  try valueExpr
 

-- syntax parsers

moduleName :: Parser [String]
moduleName =
  do sepBy (many (oneOf (upperCase ++ lowerCase ++ numeric))) (oneOf ".")
     

importStmt :: Parser Expr
importStmt =
  try (do reserved "import"
          mod <- moduleName
          whiteSpace >> reserved "as"
          as <- moduleName
          return $ Import mod as)
  <|>
  try (do reserved "import"
          mod <- moduleName
          return $ Import mod mod)

defprocStmt = try defprocFun <|> try defprocVar

defprocVar :: Parser Expr
defprocVar =
  do var <- identifier
     reservedOp "="
     reserved "do"
     exprs <- many $ try (do expr <- whiteSpace >> expression
                             reservedOp ";"
                             return expr)
     return $ Defproc (Name var) [] exprs Skip

defprocFun :: Parser Expr
defprocFun =
  do var <- identifier
     params <- parens idList
     reservedOp "="
     reserved "do"
     exprs <- many $ try (do expr <- whiteSpace >> expression
                             reservedOp ";"
                             return expr)
     return $ Defproc (Name var) params exprs Skip

defunStmt :: Parser Expr
defunStmt =
  do var <- identifier
     params <- parens idList
     reservedOp "="
     expr <- expression
     return $ Defun (Name var) params expr Skip

eagerStmt :: Parser Expr
eagerStmt =
  do var <- identifier
     w <- whiteSpace
     reservedOp ":="
     w <- whiteSpace
     expr <- expression
     return $ EagerDef (Name var) expr Skip
     
accumulateStmt :: Parser Expr
accumulateStmt =
  do var <- identifier
     reservedOp "+="
     expr <- expression
     return $ EagerDef (Name var) (Add (Var (Name var)) (expr)) Skip
     
assignStmt :: Parser Expr
assignStmt =
  do var <- identifier
     reservedOp "="
     expr1 <- expression
     return $ Def (Name var) expr1 Skip
           
ifStmt :: Parser Expr
ifStmt =
  do reserved "if"
     cond  <- expression
     reserved "then"
     expr1 <- expression
     reserved "else"
     expr2 <- expression
     return $ If cond expr1 expr2
     
caseStmt :: Parser Expr
caseStmt =
  do reserved "case"
     check <- whiteSpace >> expression
     reserved "of"
     cases <- sepBy (do cond <- whiteSpace >> identifierOrValue
                        reservedOp ":"
                        expr <- whiteSpace >> expression
                        return $ (cond, expr)
                        ) (oneOf ",")
     return $ Case check cases
     
skipStmt :: Parser Expr
skipStmt = reserved "skip" >> return Skip

readStmt :: Parser Expr
readStmt =
  do reserved "read"
     expr <- parens expression
     return $ FileRead (expr)
writeStmt :: Parser Expr
writeStmt =
  do reserved "write"
     symbol "("
     file <- expression
     symbol ","
     expr <- expression
     symbol ")"
     return $ FileWrite file expr
appendStmt :: Parser Expr
appendStmt =
  do reserved "append"
     symbol "("
     file <- expression
     symbol ","
     expr <- expression
     symbol ")"
     return $ FileAppend file expr

printStmt :: Parser Expr
printStmt =
  do reserved "print"
     expr <- expression
     return $ Output (expr)
     
inputStmt :: Parser Expr
inputStmt =
  do reserved "input"
     return $ Input
     
threadStmt :: Parser Expr
threadStmt =
  do reserved "thread"
     expr <- expression
     return $ Val $ Thread expr
     
rangeStmt :: Parser Expr
rangeStmt =
  try (do reservedOp "["
          expr1 <- expression
          reservedOp ".."
          expr2 <- expression
          reservedOp ","
          expr3 <- expression
          reservedOp "]"
          return $ Range expr1 expr2 expr3)
  <|> try (do reservedOp "["
              expr1 <- expression
              reservedOp ".."
              expr2 <- expression
              reservedOp "]"
              return $ Range expr1 expr2 (Val (NumInt 1)))

inStmt :: Parser (String, Expr)
inStmt = 
  do iterator <- identifier
     reserved "in"
     list <- expression
     reservedOp ","
     return (iterator, list)
     
nestedListComp (h:t) expr = For (Name (fst h)) (snd h) (nestedListComp t expr)
nestedListComp [] expr = expr

listCompStmt :: Parser Expr
listCompStmt =
  do reserved "for"
     ins <- many (try inStmt)
     expr <- expression
     return $ nestedListComp ins expr
     
forStmt = brackets listCompStmt
     
notStmt :: Parser Expr
notStmt =
  do reserved "not"
     expr <- expression
     return $ Not expr
     
conversionStmt :: Parser Expr
conversionStmt = try toIntStmt <|> try toFloatStmt <|> toStrStmt

toIntStmt =
  do reserved "int"
     expr <- parens expression
     return $ ToInt expr
toFloatStmt =
  do reserved "float"
     expr <- parens expression
     return $ ToFloat expr
toStrStmt =
  do reserved "str"
     expr <- parens expression
     return $ ToStr expr
     
-- value parsers

exprList :: Parser [Expr]
exprList = sepBy (whiteSpace >> expression) (oneOf ",")

identifierOrValue :: Parser Id
identifierOrValue = try idAtom <|> try idSplit <|> try idName <|> try idPattern <|> parens identifierOrValue
idAtom =
  do initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     id <- try (whiteSpace >> parens (sepBy (whiteSpace >> identifierOrValue) (oneOf ","))) <|> 
           try (do id <- whiteSpace >> identifierOrValue
                   return [id]) <|>
           do return []
     return $ AtomMatch (initial : chars) id
idSplit = 
  do id1 <- identifier
     reservedOp "+"
     id2 <- identifier
     return $ Split id1 id2
idName =
  do id <- identifier
     return $ Name id 
idPattern =
  do val <- value
     return $ Pattern val
       

idList :: Parser [Id]
idList = do id <- sepBy (whiteSpace >> identifierOrValue) (oneOf ",")
            return $ id

lambdaStmt :: Parser Expr
lambdaStmt =
  do ids <- idList
     reservedOp "->"
     expr <- expression     
     return $ Val $ Lambda ids expr

fileStmt :: Parser Expr
fileStmt =
  do reservedOp "<<"
     expr <- expression
     reservedOp ">>"
     return $ FileObj expr

strValue :: Parser Value
strValue = 
  do quote <- oneOf "\"'"
     chars <- many (do char <- noneOf [quote]
                       get <- case char of
                                '\\' -> do char' <- noneOf ""
                                           return $ case char' of                                                      
                                                      'n' -> '\n'
                                                      'r' -> '\r'
                                                      't' -> '\t'
                                                      'a' -> '\a'
                                                      'b' -> '\b'
                                                      'f' -> '\f'
                                                      'v' -> '\v'
                                                      '\\' -> '\\'
                                                      otherwise -> otherwise
                                        <|> do return '\\'
                                otherwise -> do return otherwise
                       return get)
     oneOf [quote]
     whiteSpace
     return $ Str chars
strStmt :: Parser Expr
strStmt =
  do str <- strValue
     return $ Val str
     
intValue :: Parser Value
intValue =
  do value <- integer
     return $ NumInt value
intStmt :: Parser Expr
intStmt =
  do value <- intValue
     return $ Val value
     
floatValue :: Parser Value
floatValue =
  do reservedOp "-"
     value <- float
     whiteSpace
     return $ NumFloat (value * (-1.0))
  <|>
  do value <- float
     whiteSpace
     return $ NumFloat value
floatStmt :: Parser Expr
floatStmt =
  do value <- floatValue
     return $ Val value

atomValue :: Parser Value
atomValue =
  do initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     val <- try (whiteSpace >> parens (sepBy (whiteSpace >> value) (oneOf ","))) <|> 
            try (do val' <- whiteSpace >> value
                    return [val']) <|>
            do return []
     return $ Atom (initial : chars) val
atomStmt :: Parser Expr
atomStmt =
  do initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     expr <- try (whiteSpace >> parens (sepBy (whiteSpace >> expression) (oneOf ","))) <|> 
             try (do expr <- whiteSpace >> expression
                     return [expr]) <|>
             do return []
     return $ AtomExpr (initial : chars) expr

procStmt :: Parser Expr
procStmt =
  do reserved "do"
     exprs <- many $ try (do expr <- whiteSpace >> expression
                             reservedOp ";"
                             return expr)
     return $ Val $ Proc exprs

listValue :: Parser Value
listValue =
  do exprs <- brackets (sepBy value (oneOf ","))
     return $ List exprs
listStmt :: Parser Expr
listStmt =
  do exprs <- brackets exprList
     return $ ListExpr exprs
     
keyValue :: Parser (String, Value)
keyValue =
  do key <- do quote <- oneOf "\"'"
               chars <- many (noneOf [quote])
               oneOf [quote]
               return chars
     whiteSpace >> symbol ":"
     value <- whiteSpace >> value
     return (key, value)
     keyValue :: Parser (String, Value)
keyExpr :: Parser (Expr, Expr)
keyExpr =
  do key <- whiteSpace >> expression
     whiteSpace >> symbol ":"
     expr <- whiteSpace >> expression
     return (key, expr)
hashValue :: Parser Value
hashValue =
  do keysValues <- braces (sepBy (whiteSpace >> keyValue) (oneOf ","))
     return $ Hash (makeHash keysValues)
hashStmt :: Parser Expr
hashStmt =
  do keysValues <- braces (sepBy (whiteSpace >> keyExpr) (oneOf ","))
     return $ HashExpr keysValues

     
funcallStmt :: Parser Expr
funcallStmt =
  do var <- identifier
     params <- parens exprList
     return $ Func (Name var) params
     
varcallStmt :: Parser Expr
varcallStmt =
  do var <- identifier
     return $ Var (Name var)

assignment :: Parser Expr
assignment = try defprocStmt <|> 
             try defunStmt <|> 
             try accumulateStmt <|> 
             try eagerStmt <|> 
             assignStmt
     
nestwhere [] wexpr = wexpr
nestwhere (h:t) wexpr = case h of
                          Defun a b c Skip -> Defun a b c (nestwhere t wexpr)
                          EagerDef a b Skip -> EagerDef a b (nestwhere t wexpr)
                          Def a b Skip -> Def a b (nestwhere t wexpr)
                          otherwise -> nestwhere t wexpr

whereStmt :: Parser Expr
whereStmt =
  do wexpr <- parens expression
     reserved "where"
     assignment <- sepBy (whiteSpace >> assignment) (oneOf ",")
     return $ nestwhere assignment wexpr

-- operator table

ltEq x y = Not (Gt x y)
gtEq x y = Not (Lt x y)
subs x y = Subs y x

operators :: [[ Operator Char st Expr ]]
operators = [[Infix  (reservedOp "@"   >> return (subs            )) AssocLeft],
             [Infix  (reservedOp "^"   >> return (Exp             )) AssocLeft],
             [Infix  (reservedOp "*"   >> return (Prod            )) AssocLeft,
              Infix  (reservedOp "/"   >> return (Div             )) AssocLeft],
             [Infix  (reservedOp "+"   >> return (Add             )) AssocLeft,
              Infix  (reservedOp "-"   >> return (Sub             )) AssocLeft],
             [Infix  (reservedOp "=="  >> return (Eq              )) AssocLeft,
              Infix  (reservedOp "<="  >> return (ltEq            )) AssocLeft,
              Infix  (reservedOp ">="  >> return (gtEq            )) AssocLeft,
              Infix  (reservedOp "not" >> return (InEq            )) AssocLeft,
              Infix  (reservedOp "!="  >> return (InEq            )) AssocLeft,
              Infix  (reservedOp ">"   >> return (Gt              )) AssocLeft,
              Infix  (reservedOp "<"   >> return (Lt              )) AssocLeft],
             [Infix  (reservedOp "and" >> return (And             )) AssocLeft,
              Infix  (reservedOp "or"  >> return (Or              )) AssocLeft,
              Infix  (reservedOp "&"   >> return (And             )) AssocLeft,
              Infix  (reservedOp "|"   >> return (Or              )) AssocLeft ],
             [Prefix (reservedOp "-"   >> return (Prod (Val (NumInt (-1)))))]
             ]

read name s = case (parse parser name s) of
                Right r -> r
                otherwise -> [(Nothing, Exception "Parse error")]
