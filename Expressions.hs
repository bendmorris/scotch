{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Publilc License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Expressions where

import Data.List
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Combinator
import Types
import Hash
import ParseBase

-- expression parsers

sws col = do pos <- getPosition
             if (sourceColumn pos) < col then fail "" else return ()

expression col = try (stmt col) <|> try (operation col) <|> try (term col)
           
operation col = buildExpressionParser (operators col) ((term col) <|> parens (term col))

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
  try (evalStmt col) <|>
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



ifStmt col =
  do sws col
     reserved "if"
     cond  <- expression col
     pos2 <- getPosition
     let col2 = sourceColumn pos2
     reserved "then"
     expr1 <- expression col2
     pos3 <- getPosition
     let col3 = sourceColumn pos3
     reserved "else"
     expr2 <- expression col3
     return $ If cond expr1 expr2
     
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
     
skipStmt col = do sws col
                  reserved "skip"
                  return Skip

readStmt col =
  do sws col
     reserved "read"
     expr <- parens (expression col)
     return $ FileRead (expr)
     
inputStmt col =
  do sws col
     reserved "input"
     return $ Input
     
threadStmt col =
  do sws col
     reserved "thread"
     expr <- expression col
     return $ Val $ Thread expr
     
rangeStmt col =
  try (do sws col
          brackets (do expr1 <- whiteSpace >> expression col
                       symbol ".."
                       expr2 <- whiteSpace >> expression col
                       symbol ","
                       expr3 <- whiteSpace >> expression col
                       return $ Range expr1 expr2 expr3))
  <|>
  try (do sws col
          brackets (do expr1 <- whiteSpace >> expression col
                       symbol ".."
                       expr2 <- whiteSpace >> expression col
                       return $ Range expr1 expr2 (Val (NumInt 1))))
  <|>
  try (do sws col
          brackets (do expr1 <- whiteSpace >> expression col
                       symbol ".."
                       symbol ","
                       expr3 <- whiteSpace >> expression col
                       return $ Range expr1 (Skip) expr3))
  <|>
  try (do sws col
          brackets (do expr1 <- whiteSpace >> expression col
                       symbol ".."
                       return $ Range expr1 (Skip) (Val (NumInt 1))))

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

inStmt col = 
  do sws col
     iterator <- identifier
     reserved "in"
     list <- expression col
     reserved ","
     return (iterator, ToList list)
listCompStmt col =
  do sws col
     reserved "for"
     ins <- many (try (inStmt col))
     expr <- expression col
     conds <- many (do symbol ","
                       cond <- whiteSpace >> expression col
                       return cond)
     return $ nestedListComp ins expr conds
     
forStmt col = brackets (listCompStmt col)
     
notStmt col =
  do sws col
     reserved "not"
     expr <- expression col
     return $ Not expr
     
conversionStmt col = try (toIntStmt col) <|> try (toFloatStmt col) <|> try (toStrStmt col) <|> (toListStmt col)

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
toListStmt col =
  do sws col 
     reserved "list"
     expr <- parens (expression col)
     return $ ToList expr
     
-- value parsers

exprList col = sepBy (whiteSpace >> expression col) (oneOf ",")

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
       

idList col = 
  do sws col
     id <- sepBy (whiteSpace >> identifierOrValue col) (oneOf ",")
     return $ id

lambdaStmt col =
  do sws col
     ids <- idList col
     reservedOp "->"
     expr <- expression col
     return $ Val $ Lambda ids expr

fileStmt col =
  do sws col
     symbol "<"
     expr <- expression col
     symbol ">"
     return $ FileObj expr

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
strStmt col =
  do sws col
     str <- strValue col
     return $ Val str
     
intValue col =
  do sws col
     val <- integer
     return $ NumInt val
intStmt col =
  do sws col
     val <- intValue col
     return $ Val val
     
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
floatStmt col =
  do sws col
     val <- floatValue col
     return $ Val val

atomValue col =
  do sws col
     initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     val <- try (whiteSpace >> parens (sepBy (whiteSpace >> value col) (oneOf ","))) <|> 
            try (do val' <- whiteSpace >> value col
                    return [val']) <|>
            do return []
     return $ Atom (initial : chars) val
atomStmt col =
  do sws col
     initial <- oneOf upperCase
     chars <- many $ oneOf $ upperCase ++ lowerCase
     expr <- try (whiteSpace >> parens (sepBy (whiteSpace >> expression col) (oneOf ","))) <|> 
             try (do expr <- whiteSpace >> expression col
                     return [expr]) <|>
             do return []
     return $ AtomExpr (initial : chars) expr
     
evalStmt col =
  do sws col
     reserved "eval"
     expr <- parens (expression col)
     return $ EvalExpr expr

procStmt col =
  do sws col
     reserved "do"
     pos <- getPosition
     let col' = sourceColumn pos
     exprs <- many $ expression col'
     return $ Val $ Proc exprs

listValue col =
  do sws col
     exprs <- brackets (sepBy (value col) (oneOf ","))
     return $ List exprs
listStmt col =
  do sws col
     exprs <- brackets (exprList col)
     return $ ListExpr exprs
     
keyValue col =
  do sws col
     key <- do quote <- oneOf "\"'"
               chars <- many (noneOf [quote])
               oneOf [quote]
               return chars
     whiteSpace >> symbol ":"
     val <- whiteSpace >> expression col
     return (key, val)
     
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
hashValue col =
  do sws col
     keysValues <- braces (sepBy (whiteSpace >> keyValue col) (oneOf ","))
     return $ Hash (makeHash keysValues)
hashStmt col =
  do sws col
     keysValues <- braces (sepBy (whiteSpace >> keyExpr col) (oneOf ","))
     return $ HashExpr keysValues

funcallStmt col =
  try (
  do sws col
     var <- identifier
     params <- parens (exprList col)
     return $ Func (Name var) params
  ) <|> try (
  do sws col
     var <- parens (customOp col)
     params <- parens (exprList col)
     return $ Func (Name var) params
  )
     
varcallStmt col =
  try (
  do sws col
     var <- identifier
     return $ Var (Name var) 
  ) <|> try (
  do sws col
     var <- parens (customOp col)
     return $ Var (Name var)
  )



-- statements
stmt col = 
  try (semicolon) <|>
  try (assignment col) <|>
  try (fileStmt col) <|>
  try (printStmt col) <|>
  try (importStmt col) <|>
  try (writeStmt col) <|>
  try (appendStmt col)
  
semicolon =
  do reserved ";"
     return $ Skip
  

printStmt col =
  do sws col
     reserved "print"
     expr <- expression col
     return $ Output (expr)

moduleName col =
  do sws col
     sepBy (many (oneOf (upperCase ++ lowerCase ++ numeric))) (oneOf ".")     

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

defOpStmt col =
  do sws col
     param1 <- whiteSpace >> identifierOrValue col
     operator <- whiteSpace >> many1 (oneOf operatorSymbol)
     param2 <- whiteSpace >> identifierOrValue col
     reservedOp "="
     pos <- getPosition
     let col' = sourceColumn pos
     expr <- expression col'
     return $ Defun (Name operator) [param1, param2] expr Skip
     
commutativeDefOpStmt col =
  do sws col
     param1 <- whiteSpace >> identifierOrValue col
     operator <- whiteSpace >> many1 (oneOf operatorSymbol)
     param2 <- whiteSpace >> identifierOrValue col
     reservedOp "<=>"
     pos <- getPosition
     let col' = sourceColumn pos
     expr <- expression col'
     return $ Defun (Name operator) [param2, param1] expr 
             (Defun (Name operator) [param1, param2] expr (Skip))

defprocStmt col = try (defprocFun col) <|> try (defprocVar col)

defprocVar col =
  do sws col
     var <- identifier
     reservedOp "="
     reserved "do"
     pos <- getPosition
     let col' = sourceColumn pos
     exprs <- many $ expression col'
     return $ Defproc (Name var) [] exprs Skip

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

defunStmt col =
  do sws col
     var <- identifier
     params <- parens (idList col)
     reservedOp "="
     pos <- getPosition
     let col' = sourceColumn pos
     expr <- expression col'
     return $ Defun (Name var) params expr Skip

eagerStmt col =
  do sws col
     var <- identifier
     reservedOp ":="
     pos <- getPosition
     let col' = sourceColumn pos
     expr <- expression col'
     return $ EagerDef (Name var) expr Skip
     
accumulateStmt col =
  do sws col
     var <- identifier
     reservedOp "+="
     pos <- getPosition
     let col' = sourceColumn pos
     expr <- expression col'
     return $ EagerDef (Name var) (Add (Var (Name var)) (expr)) Skip
     
assignStmt col =
  do sws col
     var <- identifier
     symbol "="
     pos <- getPosition
     let col' = sourceColumn pos
     expr1 <- expression col'
     return $ Def (Name var) expr1 Skip

assignment col = 
  try (defprocStmt col) <|>              
  try (defunStmt col) <|> 
  try (accumulateStmt col) <|> 
  try (eagerStmt col) <|> 
  try (assignStmt col) <|>
  try (commutativeDefOpStmt col) <|>
  try (defOpStmt col)
     
writeStmt col =
  do sws col
     reserved "write"
     symbol "("
     file <- expression col
     symbol ","
     expr <- expression col
     symbol ")"
     return $ FileWrite file expr
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
  do whiteSpace
     sws col
     reserved "where"
     assignment <- sepBy1 (whiteSpace >> assignment col) (oneOf ",")
     return $ nestwhere assignment
     
lambdaCallStmt col =
  do whiteSpace
     reservedOp "<-"
     args <- sepBy1 (whiteSpace >> expression col) (oneOf ",")
     return $ lambdaCall args
     
lambdaCall args expr = LambdaCall expr args

customOp col = 
  do whiteSpace
     sws col
     op <- many1 (oneOf operatorSymbol)
     whiteSpace
     if isInfixOf [op] forbiddenOps
      then fail op
      else return op

opCall op expr1 expr2 = Func (Name op) [expr1, expr2]

rsvdOp col op = do sws col
                   reservedOp op

operators col = 
  [[Infix  (rsvdOp col "@"   >> return (subs            )) AssocLeft],
   [Infix  (rsvdOp col "^"   >> return (Exp             )) AssocLeft],
   [Infix  (rsvdOp col "mod" >> return (Mod             )) AssocLeft,
    Infix  (rsvdOp col "%"   >> return (Mod             )) AssocLeft],
   [Infix  (rsvdOp col "*"   >> return (Prod            )) AssocLeft,
    Infix  (rsvdOp col "/"   >> return (Div             )) AssocLeft],
   [Infix  (rsvdOp col "+"   >> return (Add             )) AssocLeft,
    Infix  (rsvdOp col "-"   >> return (Sub             )) AssocLeft],
   [Infix  (rsvdOp col "=="  >> return (Eq              )) AssocLeft,
    Infix  (rsvdOp col "<="  >> return (ltEq            )) AssocLeft,
    Infix  (rsvdOp col ">="  >> return (gtEq            )) AssocLeft,
    Infix  (rsvdOp col "not" >> return (InEq            )) AssocLeft,
    Infix  (rsvdOp col "!="  >> return (InEq            )) AssocLeft,
    Infix  (rsvdOp col ">"   >> return (Gt              )) AssocLeft,
    Infix  (rsvdOp col "<"   >> return (Lt              )) AssocLeft],
   [Infix  (rsvdOp col "and" >> return (And             )) AssocLeft,
    Infix  (rsvdOp col "or"  >> return (Or              )) AssocLeft,
    Infix  (rsvdOp col "&"   >> return (And             )) AssocLeft,
    Infix  (rsvdOp col "|"   >> return (Or              )) AssocLeft],
   [Prefix (rsvdOp col "-"   >> return (Prod (Val (NumInt (-1)))))],
   [Postfix(do { l <- lambdaCallStmt col; return (l     )})          ],
   [Infix  (do { op <- customOp col;return (opCall op   )}) AssocLeft],
   [Postfix(do { w <- whereStmt col;return (w           )})          ]
   ]
