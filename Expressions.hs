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

module Expressions where

import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Types
import Hash
import ParseBase

-- expression parsers

sws col = do pos <- getPosition
             if (sourceColumn pos) < col then fail "" else return ()

expression :: Column -> Parser Expr
expression col = try (stmt col) <|> try (operation col) <|> try (term col)
           
operation col = buildExpressionParser (operators col) ((term col) <|> parens (term col))

term :: Column -> Parser Expr
term col = try (valueExpr col) <|>
           try (parens (expression col))


reservedWord col = try (do sws col
                           reserved "true"
                           return (Bit True))
                   <|>
                   try (do sws col
                           reserved "false"
                           return (Bit False))
                   <|>
                   try (do sws col
                           reserved "null"
                           return (Null))
reservedExpr col =
  do word <- reservedWord col
     return $ Val word

value col = 
  try (reservedWord col) <|>
  try (atomValue col) <|>
  try (hashValue col) <|>
  try (listValue col) <|> 
  try (strValue col) <|> 
  try (floatValue col) <|> 
  try (intValue col)
valueStmt col =
  try (reservedExpr col) <|>
  try (atomStmt col) <|>
  try (procStmt col) <|>
  try (hashStmt col) <|>
  try (listStmt col) <|>
  try (lambdaStmt col) <|>
  try (strStmt col) <|>
  try (floatStmt col) <|>
  try (intStmt col)
valueExpr col = 
  try (fileStmt col) <|>
  try (ifStmt col) <|>
  try (caseStmt col) <|>
  try (skipStmt col) <|>
  try (readStmt col) <|>
  try (inputStmt col) <|>
  try (threadStmt col) <|>
  try (rangeStmt col) <|>
  try (takeStmt col) <|>
  try (forStmt col) <|>
  try (notStmt col) <|>
  try (conversionStmt col) <|>
  try (valueStmt col) <|>
  try (funcallStmt col) <|>
  try (varcallStmt col)



ifStmt :: Column -> Parser Expr
ifStmt col =
  do sws col
     reserved "if"
     cond  <- expression col
     reserved "then"
     expr1 <- expression col
     reserved "else"
     expr2 <- expression col
     return $ If cond expr1 expr2
     
caseStmt :: Column -> Parser Expr
caseStmt col =
  do sws col
     reserved "case"
     check <- whiteSpace >> expression col
     reserved "of"
     cases <- sepBy (do cond <- whiteSpace >> identifierOrValue col
                        reservedOp ":"
                        pos <- getPosition
                        expr <- whiteSpace >> expression (sourceColumn pos)
                        return $ (cond, expr)
                        ) (oneOf ",")
     return $ Case check cases
     
skipStmt :: Column -> Parser Expr
skipStmt col = do sws col
                  reserved "skip"
                  return Skip

readStmt :: Column -> Parser Expr
readStmt col =
  do sws col
     reserved "read"
     expr <- parens (expression col)
     return $ FileRead (expr)
     
inputStmt :: Column -> Parser Expr
inputStmt col =
  do sws col
     reserved "input"
     return $ Input
     
threadStmt :: Column -> Parser Expr
threadStmt col =
  do sws col
     reserved "thread"
     expr <- expression col
     return $ Val $ Thread expr
     
rangeStmt :: Column -> Parser Expr
rangeStmt col =
  try (do sws col
          reservedOp "["
          expr1 <- whiteSpace >> expression col
          reservedOp ".."
          expr2 <- whiteSpace >> expression col
          reserved ","
          expr3 <- whiteSpace >> expression col
          reservedOp "]"
          return $ Range expr1 expr2 expr3)
  <|> try (do sws col
              reservedOp "["
              expr1 <- expression col
              reservedOp ".."
              expr2 <- expression col
              reservedOp "]"
              return $ Range expr1 expr2 (Val (NumInt 1)))

takeStmt :: Column -> Parser Expr
takeStmt col =
  do sws col
     reserved "take"
     expr1 <- expression col
     reserved "from"
     expr2 <- expression col
     return $ Take expr1 expr2
     
nestedListComp (h:t) expr conds = For (Name (fst h)) (snd h) (nestedListComp t expr conds)
                                    (if t == [] then conds else [])
nestedListComp [] expr conds = expr

inStmt :: Column -> Parser (String, Expr)
inStmt col = 
  do sws col
     iterator <- identifier
     reserved "in"
     list <- expression col
     reserved ","
     return (iterator, list)
listCompStmt :: Column -> Parser Expr
listCompStmt col =
  do sws col
     reserved "for"
     ins <- many (try (inStmt col))
     expr <- expression col
     conds <- many (do reserved ","
                       cond <- whiteSpace >> expression col
                       return cond)
     return $ nestedListComp ins expr conds
     
forStmt :: Column -> Parser Expr
forStmt col = brackets (listCompStmt col)
     
notStmt :: Column -> Parser Expr
notStmt col =
  do sws col
     reserved "not"
     expr <- expression col
     return $ Not expr
     
conversionStmt :: Column -> Parser Expr
conversionStmt col = try (toIntStmt col) <|> try (toFloatStmt col) <|> (toStrStmt col)

toIntStmt col =
  do sws col
     reserved "int"
     expr <- parens (expression col)
     return $ ToInt expr
toFloatStmt col =
  do sws col
     reserved "float"
     expr <- parens (expression col)
     return $ ToFloat expr
toStrStmt col =
  do sws col 
     reserved "str"
     expr <- parens (expression col)
     return $ ToStr expr
     
-- value parsers

exprList :: Column -> Parser [Expr]
exprList col = sepBy (whiteSpace >> expression col) (oneOf ",")

identifierOrValue :: Column -> Parser Id
identifierOrValue col = 
  try (idAtom col) <|> 
  try (idSplit col) <|> 
  try (idName col) <|> 
  try (idPattern col) <|> 
  parens (identifierOrValue col)
idAtom col =
  do sws col
     initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     id <- try (whiteSpace >> parens (sepBy (whiteSpace >> identifierOrValue col) (oneOf ","))) <|> 
           try (do id <- whiteSpace >> identifierOrValue col
                   return [id]) <|>
           do return []
     return $ AtomMatch (initial : chars) id
idSplit col = 
  do sws col
     id1 <- identifier
     reservedOp "+"
     id2 <- identifier
     return $ Split id1 id2
idName col =
  do sws col
     id <- identifier
     return $ Name id 
idPattern col =
  do sws col
     val <- value col
     return $ Pattern val
       

idList :: Column -> Parser [Id]
idList col = 
  do sws col
     id <- sepBy (whiteSpace >> identifierOrValue col) (oneOf ",")
     return $ id

lambdaStmt :: Column -> Parser Expr
lambdaStmt col =
  do sws col
     ids <- idList col
     reservedOp "->"
     expr <- expression col
     return $ Val $ Lambda ids expr

fileStmt :: Column -> Parser Expr
fileStmt col =
  do sws col
     reservedOp "<"
     expr <- expression col
     reservedOp ">"
     return $ FileObj expr

strValue :: Column -> Parser Value
strValue col = 
  do sws col
     quote <- oneOf "\"'"
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
strStmt :: Column -> Parser Expr
strStmt col =
  do sws col
     str <- strValue col
     return $ Val str
     
intValue :: Column -> Parser Value
intValue col =
  do sws col
     val <- integer
     return $ NumInt val
intStmt :: Column -> Parser Expr
intStmt col =
  do sws col
     val <- intValue col
     return $ Val val
     
floatValue :: Column -> Parser Value
floatValue col =
  do sws col
     reservedOp "-"
     val <- float
     whiteSpace
     return $ NumFloat (val * (-1.0))
  <|>
  do sws col
     val <- float
     whiteSpace
     return $ NumFloat val
floatStmt :: Column -> Parser Expr
floatStmt col =
  do sws col
     val <- floatValue col
     return $ Val val

atomValue :: Column -> Parser Value
atomValue col =
  do sws col
     initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     val <- try (whiteSpace >> parens (sepBy (whiteSpace >> value col) (oneOf ","))) <|> 
            try (do val' <- whiteSpace >> value col
                    return [val']) <|>
            do return []
     return $ Atom (initial : chars) val
atomStmt :: Column -> Parser Expr
atomStmt col =
  do sws col
     initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     expr <- try (whiteSpace >> parens (sepBy (whiteSpace >> expression col) (oneOf ","))) <|> 
             try (do expr <- whiteSpace >> expression col
                     return [expr]) <|>
             do return []
     return $ AtomExpr (initial : chars) expr

procStmt :: Column -> Parser Expr
procStmt col =
  do sws col
     reserved "do"
     pos <- getPosition
     let col' = sourceColumn pos
     exprs <- many $ expression col'
     return $ Val $ Proc exprs

listValue :: Column -> Parser Value
listValue col =
  do sws col
     exprs <- brackets (sepBy (value col) (oneOf ","))
     return $ List exprs
listStmt :: Column -> Parser Expr
listStmt col =
  do sws col
     exprs <- brackets (exprList col)
     return $ ListExpr exprs
     
keyValue :: Column -> Parser (String, Value)
keyValue col =
  do sws col
     key <- do quote <- oneOf "\"'"
               chars <- many (noneOf [quote])
               oneOf [quote]
               return chars
     whiteSpace >> symbol ":"
     val <- whiteSpace >> value col
     return (key, val)
     
keyExpr :: Column -> Parser (Expr, Expr)
keyExpr col = try (
  do sws col
     key <- whiteSpace >> (many (oneOf (upperCase ++ lowerCase)))
     whiteSpace >> symbol "="
     expr <- whiteSpace >> expression col
     return (Val (Str key), expr)
  ) <|> (
  do sws col
     key <- whiteSpace >> expression col
     whiteSpace >> symbol ":"
     expr <- whiteSpace >> expression col
     return (key, expr)
  )
hashValue :: Column -> Parser Value
hashValue col =
  do sws col
     keysValues <- braces (sepBy (whiteSpace >> keyValue col) (oneOf ","))
     return $ Hash (makeHash keysValues)
hashStmt :: Column -> Parser Expr
hashStmt col =
  do sws col
     keysValues <- braces (sepBy (whiteSpace >> keyExpr col) (oneOf ","))
     return $ HashExpr keysValues

funcallStmt :: Column -> Parser Expr
funcallStmt col =
  do sws col
     var <- identifier
     params <- parens (exprList col)
     return $ Func (Name var) params
     
varcallStmt :: Column -> Parser Expr
varcallStmt col =
  do sws col
     var <- identifier
     return $ Var (Name var) 



-- statements

stmt col = 
  try (fileStmt col) <|>
  try (printStmt col) <|>
  try (importStmt col) <|>
  try (assignment col) <|>
  try (writeStmt col) <|>
  try (appendStmt col)
  

printStmt :: Column -> Parser Expr
printStmt col =
  do sws col
     reserved "print"
     expr <- expression col
     return $ Output (expr)

moduleName :: Column -> Parser [String]
moduleName col =
  do sws col
     sepBy (many (oneOf (upperCase ++ lowerCase ++ numeric))) (oneOf ".")     

importStmt :: Column -> Parser Expr
importStmt col =
  try (do sws col
          reserved "import"
          mod <- moduleName col
          whiteSpace >> reserved "as"
          as <- moduleName col
          return $ Import mod as)
  <|>
  try (do sws col
          reserved "import"
          mod <- moduleName col
          return $ Import mod mod)

defOpStmt :: Column -> Parser Expr
defOpStmt col =
  do sws col
     param1 <- whiteSpace >> identifierOrValue col
     operator <- whiteSpace >> many1 (oneOf operatorSymbol)
     param2 <- whiteSpace >> identifierOrValue col
     reservedOp "="
     expr <- expression col
     return $ Defun (Name operator) [param1, param2] expr Skip
     
commutativeDefOpStmt :: Column -> Parser Expr
commutativeDefOpStmt col =
  do sws col
     param1 <- whiteSpace >> identifierOrValue col
     operator <- whiteSpace >> many1 (oneOf operatorSymbol)
     param2 <- whiteSpace >> identifierOrValue col
     reservedOp "<=>"
     expr <- expression col
     return $ Defun (Name operator) [param2, param1] expr 
             (Defun (Name operator) [param1, param2] expr (Skip))

defprocStmt col = try (defprocFun col) <|> try (defprocVar col)

defprocVar :: Column -> Parser Expr
defprocVar col =
  do sws col
     var <- identifier
     reservedOp "="
     reserved "do"
     pos <- getPosition
     let col' = sourceColumn pos
     exprs <- many $ expression col'
     return $ Defproc (Name var) [] exprs Skip

defprocFun :: Column -> Parser Expr
defprocFun col =
  do sws col
     var <- identifier
     params <- parens (idList col)
     reservedOp "="
     reserved "do"
     pos <- getPosition
     let col' = sourceColumn pos
     exprs <- many $ expression col'
     return $ Defproc (Name var) params exprs Skip

defunStmt :: Column -> Parser Expr
defunStmt col =
  do sws col
     var <- identifier
     params <- parens (idList col)
     reservedOp "="
     expr <- expression col
     return $ Defun (Name var) params expr Skip

eagerStmt :: Column -> Parser Expr
eagerStmt col =
  do sws col
     var <- identifier
     w <- whiteSpace
     reservedOp ":="
     w <- whiteSpace
     expr <- expression col
     return $ EagerDef (Name var) expr Skip
     
accumulateStmt :: Column -> Parser Expr
accumulateStmt col =
  do sws col
     var <- identifier
     reservedOp "+="
     expr <- expression col
     return $ EagerDef (Name var) (Add (Var (Name var)) (expr)) Skip
     
assignStmt :: Column -> Parser Expr
assignStmt col =
  do sws col
     var <- identifier
     reservedOp "="
     expr1 <- expression col
     return $ Def (Name var) expr1 Skip

assignment :: Column -> Parser Expr
assignment col = 
  try (defprocStmt col) <|>              
  try (defunStmt col) <|> 
  try (accumulateStmt col) <|> 
  try (eagerStmt col) <|> 
  try (assignStmt col) <|>
  try (commutativeDefOpStmt col) <|>
  try (defOpStmt col)
     
writeStmt :: Column -> Parser Expr
writeStmt col =
  do sws col
     reserved "write"
     symbol "("
     file <- expression col
     symbol ","
     expr <- expression col
     symbol ")"
     return $ FileWrite file expr
appendStmt :: Column -> Parser Expr
appendStmt col =
  do sws col
     reserved "append"
     symbol "("
     file <- expression col
     symbol ","
     expr <- expression col
     symbol ")"
     return $ FileAppend file expr



-- operator table

ltEq x y = Not (Gt x y)
gtEq x y = Not (Lt x y)
subs x y = Subs y x

nestwhere [] wexpr = wexpr
nestwhere (h:t) wexpr = case h of
                          Defun a b c Skip -> Defun a b c (nestwhere t wexpr)
                          EagerDef a b Skip -> EagerDef a b (nestwhere t wexpr)
                          Def a b Skip -> Def a b (nestwhere t wexpr)
                          otherwise -> nestwhere t wexpr

whereStmt col =
  do sws col
     whiteSpace
     reserved "where"
     assignment <- sepBy1 (whiteSpace >> assignment col) (oneOf ",")
     return $ nestwhere assignment

customOp col = 
  do sws col
     whiteSpace
     op <- many1 (oneOf operatorSymbol)
     whiteSpace
     if isInfixOf [op] forbiddenOps
      then fail op
      else return op

opCall op expr1 expr2 = Func (Name op) [expr1, expr2]

operators col = 
  [[Infix  (reservedOp "@"   >> return (subs            )) AssocLeft],
   [Infix  (reservedOp "^"   >> return (Exp             )) AssocLeft],
   [Infix  (reservedOp "mod" >> return (Mod             )) AssocLeft,
    Infix  (reservedOp "%"   >> return (Mod             )) AssocLeft],
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
    Infix  (reservedOp "|"   >> return (Or              )) AssocLeft],
   [Prefix (reservedOp "-"   >> return (Prod (Val (NumInt (-1)))))],
   [Infix  (do { op <- customOp col;return (opCall op   )}) AssocLeft],
   [Postfix(do { w <- whereStmt col;return (w           )})          ]
   ]
