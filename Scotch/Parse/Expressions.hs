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

module Scotch.Parse.Expressions where

import Data.List
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Combinator
import Scotch.Types.Types
import Scotch.Types.Hash
import Scotch.Parse.ParseBase

-- expression parsers

sws col = do pos <- getPosition
             if (sourceColumn pos) < col then fail "" else return ()

expression col = try (stmt col) <|> 
                 try (operation col) <|> 
                 try (term col)
           
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
  try (hashValue col) <|>
  try (strValue col) <|> 
  try (floatValue col) <|> 
  try (intValue col)
valueStmt col =
  try (reservedExpr col) <|>
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
  try (varcallStmt col) <|>
  try (valueStmt col)



ifStmt col =
  try
  (do sws col
      reserved "if"
      cond <- expression col
      pos2 <- getPosition
      let col2 = sourceColumn pos2
      reserved "then"
      expr1 <- expression col2
      pos3 <- getPosition
      let col3 = sourceColumn pos3
      reserved "else"
      expr2 <- expression col3
      return $ If cond expr1 expr2)
  <|>
  (do sws col
      reserved "if"
      cond <- expression col
      pos2 <- getPosition
      let col2 = sourceColumn pos2
      reserved "then"
      expr <- expression col2
      return $ If cond expr Skip)
     
caseStmt col =
  do sws col
     reserved "case"
     check <- whiteSpace >> expression col
     reserved "of"
     cases <- sepBy1 (do cond <- whiteSpace >> expression col
                         reservedOp "->"
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
     
nestedListComp (h:t) expr conds = For (fst h) (snd h) (nestedListComp t expr conds)
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
idList col = sepBy (do sws col
                       id <- whiteSpace >> identifier
                       return $ id) (oneOf ",")

lambdaStmt col =
  do sws col
     ids <- parens (idList col)
     reservedOp "->"
     expr <- expression col
     return $ Val $ Lambda ids expr

fileStmt col =
  do sws col
     symbol "file"
     expr <- parens (expression col)
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

listStmt col =
  do sws col
     exprs <- brackets (exprList col)
     return $ List exprs
     
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
     return $ Hash (makeHash strHash keysValues emptyHash)
hashStmt col =
  do sws col
     keysValues <- braces (sepBy (whiteSpace >> keyExpr col) (oneOf ","))
     return $ HashExpr keysValues

varcallStmt col =
  try (
  do sws col
     var <- identifier
     return $ Var var
  ) <|> (
  do sws col
     var <- parens (customOp col)
     return $ Var var
  ) <|> (
  do sws col
     n <- intStmt col
     var <- identifier
     return $ Prod n (Var var)
  ) <|> (
  do sws col
     n <- floatStmt col
     var <- identifier
     return $ Prod n (Var var)
  )
     

-- statements
stmt col = 
  try (semicolon) <|>
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
                          EagerDef a b Skip -> EagerDef a b (nestwhere t wexpr)
                          Def a b Skip -> Def a b (nestwhere t wexpr)
                          otherwise -> nestwhere t wexpr

whereStmt col =
  do whiteSpace
     sws col
     reserved "where"
     assignment <- sepBy1 (whiteSpace >> buildExpressionParser [assignments col] (expression col)) (oneOf ",")
     return $ nestwhere assignment
     
callStmt col =
  do sws col
     args <- many1 (parens $ exprList col)
     return $ callPostfix args
callPostfix [] id = id
callPostfix (h:t) id = callPostfix t (Call id h)

customOp col = 
  do whiteSpace
     sws col
     op <- many1 (oneOf operatorSymbol)
     whiteSpace
     if isInfixOf [op] forbiddenOps
      then fail op
      else return op

opCall op expr1 expr2 = Call (Var op) [expr1, expr2]

rsvdOp col op = do sws col
                   reservedOp op
                   
assignment a b = Def a b Skip
eagerAssign a b = EagerDef a b Skip
accumulate a b = EagerDef a (Add a b) Skip

assignments col = 
   [Infix  (rsvdOp col "="   >> return (assignment      )) AssocNone,
    Infix  (rsvdOp col ":="  >> return (eagerAssign     )) AssocNone,
    Infix  (rsvdOp col "+="  >> return (accumulate      )) AssocNone]

operators col = 
  [[Postfix(do { c <- callStmt col; return (c          )})          ],
   [Infix  (rsvdOp col "@"   >> return (subs            )) AssocLeft],
   [Infix  (rsvdOp col "^"   >> return (Exp             )) AssocLeft],
   [Infix  (rsvdOp col "mod" >> return (Mod             )) AssocLeft,
    Infix  (rsvdOp col "%"   >> return (Mod             )) AssocLeft],
   [Infix  (rsvdOp col "*"   >> return (Prod            )) AssocLeft,
    Infix  (rsvdOp col "/"   >> return (Div             )) AssocLeft],
   [Infix  (rsvdOp col "+"   >> return (Add             )) AssocLeft,
    Infix  (rsvdOp col "-"   >> return (Sub             )) AssocLeft],
   [Infix  (rsvdOp col ":"   >> return (Concat          )) AssocLeft],
   [Infix  (rsvdOp col "=="  >> return (Eq              )) AssocLeft,
    Infix  (rsvdOp col "is"  >> return (Eq              )) AssocLeft,
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
   [Postfix(do { w <- whereStmt col;return (w          )})          ],
   assignments col,
   [Infix  (do { op <- customOp col;return (opCall op   )}) AssocLeft]
   ]
