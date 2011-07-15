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
assign op f =
  do a <- expression
     symbol op
     b <- expression
     return $ f a b
normalAssign a b = Def a b Skip
eagerAssign a b = EagerDef a b Skip
addAssign a b = EagerDef a (Add a b) Skip
subAssign a b = EagerDef a (Sub a b) Skip
prodAssign a b = EagerDef a (Prod a b) Skip
divAssign a b = EagerDef a (Div a b) Skip
expAssign a b = EagerDef a (Exp a b) Skip
modAssign a b = EagerDef a (Mod a b) Skip

assignment = 
  try (assign "=" normalAssign) <|>
  try (assign ":=" eagerAssign) <|>
  try (assign "+=" addAssign) <|>
  try (assign "-=" subAssign) <|>
  try (assign "*=" prodAssign) <|>
  try (assign "/=" divAssign) <|>
  try (assign "^=" expAssign) <|>
  try (assign "%=" modAssign)

statement = assignment <|> expression

expression = try (operation True) <|> 
             term
nonCurryExpression = try (operation False) <|>
                     term
           
operation b = buildExpressionParser (operators b) (term <|> parens term)

term = try valueExpr <|>
       try stmt <|>
       try (parens expression)

reservedWord = try (do reserved "true"
                       return (Bit True))
               <|>
               try (do reserved "false"
                       return (Bit False))
               <|>
               try (do reserved "null"
                       return (Null))
reservedExpr =
  do word <- reservedWord
     return $ Val word

value = 
  try reservedWord <|>
  try strValue <|> 
  try floatValue <|> 
  try intValue
valueStmt =
  try reservedExpr <|>
  try evalStmt <|>
  try procStmt <|>
  try hashStmt <|>
  try listStmt <|>
  try lambdaStmt <|>
  try strStmt <|>
  try floatStmt <|>
  try intStmt
valueExpr = 
  try ruleStmt <|>
  try useRuleStmt <|>
  try fileStmt <|>
  try ifStmt <|>
  try caseStmt <|>
  try skipStmt <|>
  try readStmt <|>
  try inputStmt <|>
  try threadStmt <|>
  try rangeStmt <|>
  try takeStmt <|>
  try forStmt <|>
  try notStmt <|>
  try conversionStmt <|>
  try varcallStmt <|>
  try valueStmt



ifStmt =
  try
  (do reserved "if"
      cond <- expression
      reserved "then"
      expr1 <- expression
      reserved "else"
      expr2 <- expression
      return $ If (ToBool cond) expr1 expr2)
  <|>
  (do reserved "if"
      cond <- expression
      reserved "then"
      expr <- expression
      return $ If cond expr Skip)
     
caseStmt =
  do reserved "case"
     check <- whiteSpace >> expression
     reserved "of"
     cases <- sepBy1 (do cond <- whiteSpace >> expression
                         reservedOp "->"
                         expr <- whiteSpace >> expression
                         return $ (cond, expr)
                         ) (oneOf ",")
     return $ Case check cases
     
skipStmt = do reserved "skip"
              return Skip

readStmt =
  do reserved "read"
     expr <- parens expression
     return $ FileRead (expr)
     
inputStmt =
  do reserved "input"
     return $ Input
     
threadStmt =
  do reserved "thread"
     expr <- expression
     return $ Val $ Thread expr
     
rangeStmt =
  try (do brackets (do expr1 <- whiteSpace >> expression
                       symbol ".."
                       expr2 <- whiteSpace >> expression
                       symbol ","
                       expr3 <- whiteSpace >> expression
                       return $ Range expr1 expr2 expr3))
  <|>
  try (do brackets (do expr1 <- whiteSpace >> expression
                       symbol ".."
                       expr2 <- whiteSpace >> expression
                       return $ Range expr1 expr2 (Val (NumInt 1))))
  <|>
  try (do brackets (do expr1 <- whiteSpace >> expression
                       symbol ".."
                       symbol ","
                       expr3 <- whiteSpace >> expression
                       return $ Range expr1 (Skip) expr3))
  <|>
  try (do brackets (do expr1 <- whiteSpace >> expression
                       symbol ".."
                       return $ Range expr1 (Skip) (Val (NumInt 1))))

takeStmt =
  do reserved "take"
     expr1 <- expression
     reserved "from"
     expr2 <- expression
     return $ Take expr1 expr2
     
nestedListComp (h:t) expr conds = For (fst h) (snd h) (nestedListComp t expr conds)
                                    (if t == [] then conds else [])
nestedListComp [] expr conds = expr

inStmt = 
  do iterator <- identifier
     reserved "in"
     list <- expression
     reserved ","
     return (iterator, ToList list)
listCompStmt =
  do reserved "for"
     ins <- many (try inStmt)
     expr <- expression
     conds <- many (do symbol ","
                       cond <- whiteSpace >> expression
                       return cond)
     return $ nestedListComp ins expr conds
     
forStmt = brackets listCompStmt
     
notStmt =
  do reserved "not"
     expr <- expression
     return $ Not expr
     
conversionStmt = 
  try toIntStmt <|> 
  try toFloatStmt <|> 
  try toStrStmt <|> 
  try toListStmt <|> 
  try toBoolStmt

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
toListStmt =
  do reserved "list"
     expr <- parens expression
     return $ ToList expr
toBoolStmt =
  do reserved "bool"
     expr <- parens expression
     return $ ToBool expr
     
-- value parsers

exprList = sepBy (whiteSpace >> expression) (oneOf ",")
idList = sepBy (do id <- whiteSpace >> identifier
                   return $ id) (oneOf ",")

lambdaStmt =
  do ids <- parens idList
     reservedOp "->"
     expr <- expression
     return $ Val $ Lambda ids expr
     
ruleStmt = 
  do symbol "rule"
     id <- whiteSpace >> identifier
     symbol "=>"
     whiteSpace
     binds <- sepBy1 rule (oneOf ",") 
     return $ Def (Var id) (Rule binds) Skip

rule =
  do x <- whiteSpace >> assignment
     y <- case x of
            Def a b Skip -> do return x
            EagerDef a b Skip -> do return x
            otherwise -> do fail ""
                            return Skip
     return y
     
useRuleStmt =
  do symbol "using"
     x <- expression
     symbol "=>"
     y <- statement
     return $ UseRule x y

fileStmt =
  do symbol "file"
     expr <- parens expression
     return $ FileObj expr

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
strStmt =
  do str <- strValue
     return $ Val str
     
intValue =
  do val <- integer
     return $ NumInt val
intStmt =
  do val <- intValue
     return $ Val val
     
floatValue =
  do reservedOp "-"
     val <- float
     return $ NumFloat (val * (-1.0))
  <|>
  do val <- float
     return $ NumFloat val
floatStmt =
  do val <- floatValue
     return $ Val val
     
evalStmt =
  do reserved "eval"
     expr <- parens expression
     return $ EvalExpr expr

procStmt =
  do reserved "do"
     initialPos <- getPosition
     exprs <- sepBy1 (do whiteSpace
                         pos <- getPosition
                         expr <- statement   
                         if sourceColumn pos < sourceColumn initialPos then fail "" else do return ()
                         return (sourceLine pos, expr))
                     (oneOf ";")
                       
     if length (nub [fst expr | expr <- exprs]) /= length (exprs) then fail "" else do return ()
     return $ Val $ Proc [snd expr | expr <- exprs]

listStmt =
  do exprs <- brackets exprList
     return $ List exprs
     
keyValue = try (
  do key <- whiteSpace >> (many (oneOf (upperCase ++ lowerCase)))
     whiteSpace >> symbol "="
     expr <- whiteSpace >> expression
     return (Val (Str key), expr)
  ) <|> (
  do key <- whiteSpace >> value
     whiteSpace >> symbol ":"
     expr <- whiteSpace >> expression
     return (Val key, expr)
  )
hashStmt =
  do keysValues <- braces (sepBy (whiteSpace >> keyValue) (oneOf ","))
     return $ HashExpr keysValues

varcallStmt =
  try (
  do var <- identifier
     return $ Var var
  ) <|> (
  do var <- parens customOp
     return $ Var var
  ) <|> (
  do n <- intStmt
     var <- identifier
     return $ Prod n (Var var)
  ) <|> (
  do n <- floatStmt
     var <- identifier
     return $ Prod n (Var var)
  )
     

-- statements
stmt = 
  try printStmt <|>
  try importStmt <|>
  try writeStmt <|>
  try appendStmt
  

printStmt =
  do reserved "print"
     expr <- try (parens $ expression) <|> do return Skip
     return $ Output (expr)

moduleName =
  do sepBy (many (oneOf (upperCase ++ lowerCase ++ numeric))) (oneOf ".")     

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

writeStmt =
  do reserved "write"
     symbol "("
     file <- expression
     symbol ","
     expr <- expression
     symbol ")"
     return $ FileWrite file expr
appendStmt =
  do reserved "append"
     symbol "("
     file <- expression
     symbol ","
     expr <- expression
     symbol ")"
     return $ FileAppend file expr



-- operator table

ltEq x y = Or (Lt x y) (Eq x y)
gtEq x y = Or (Gt x y) (Eq x y)
subs x y = Subs y x

nestwhere [] wexpr = wexpr
nestwhere (h:t) wexpr = case h of
                          EagerDef a b Skip -> EagerDef a b (nestwhere t wexpr)
                          Def a b Skip -> Def a b (nestwhere t wexpr)
                          otherwise -> nestwhere t wexpr

whereStmt =
  do reserved "where"
     assignment <- sepBy1 (whiteSpace >> assignment) (oneOf ",")
     return $ nestwhere assignment
     
callStmt =
  do args <- many1 (parens exprList)
     return $ callPostfix args
curryStmt =
  do args <- many1 (nonCurryExpression)
     return $ callPostfix [args]
     
callPostfix [] id = id
callPostfix (h:t) id = callPostfix t (Call id h)

customOp = 
  do whiteSpace
     op <- many1 (oneOf operatorSymbol)
     whiteSpace
     if isInfixOf [op] forbiddenOps
      then fail op
      else return op

opCall op expr1 expr2 = Call (Var op) [expr1, expr2]

operators b = 
  [[Prefix (reservedOp "-"   >> return (Prod (Val (NumInt (-1)))))],
   [Postfix(do { c <- callStmt; return (c          )})          ],
   [Infix  (reservedOp "@"   >> return (subs            )) AssocLeft],
   [Infix  (reservedOp "^"   >> return (Exp             )) AssocLeft],
   [Infix  (reservedOp "mod" >> return (Mod             )) AssocLeft,
    Infix  (reservedOp "%"   >> return (Mod             )) AssocLeft],
   [Infix  (reservedOp "*"   >> return (Prod            )) AssocLeft,
    Infix  (reservedOp "/"   >> return (Div             )) AssocLeft],
   [Infix  (reservedOp "+"   >> return (Add             )) AssocLeft,
    Infix  (reservedOp "-"   >> return (Sub             )) AssocLeft],
   [Infix  (reservedOp ":"   >> return (Concat          )) AssocLeft],
   [Infix  (reservedOp "=="  >> return (Eq              )) AssocLeft,
    Infix  (reservedOp "is"  >> return (Eq              )) AssocLeft,
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
   [Postfix(do { w <- whereStmt;return (w          )})          ]
   --[Infix  (do { op <- customOp;return (opCall op   )}) AssocLeft]
   ] ++ if b 
        then [[Postfix(do { c <- curryStmt;return (c          )})          ]]
        else []
