module Read where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Types

languageDef =
  emptyDef { Token.commentStart    = "#-",
             Token.commentEnd      = "-#",
             Token.commentLine     = "#",
             Token.identStart      = letter,
             Token.identLetter     = alphaNum,
             Token.reservedNames   = [
                                      "if",
                                      "then",
                                      "else",
                                      "for",
                                      "in",
                                      "skip",
                                      "true",
                                      "false",
                                      "and",
                                      "or",
                                      "not",
                                      "print"
                                     ],
             Token.reservedOpNames = ["+", "-", "*", "/", "^", "=", ":=", "==",
                                      "<", ">", "and", "or", "not"
                                     ]
           }
           
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
float      = Token.float      lexer -- parses a float
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parser :: Parser Expr
parser = whiteSpace >> statement
           
ifStmt :: Parser Expr
ifStmt =
  do reserved "if"
     cond  <- expression
     reserved "then"
     stmt1 <- expression
     reserved "else"
     stmt2 <- expression
     return $ If cond stmt1 stmt2
 
assignStmt :: Parser Expr
assignStmt =
  do var <- identifier
     w <- whiteSpace
     reservedOp "="
     w <- whiteSpace
     expr1 <- expression
     return $ Def var expr1 (Var var)
     
printStmt :: Parser Expr
printStmt =
  do reservedOp "print"
     expr <- expression
     return $ Output (expr) (expr)

exprList :: Parser [Expr]
exprList = sepBy expression (oneOf ",")

idList :: Parser [Id]
idList = sepBy identifier (oneOf ",")

strStmt :: Parser Expr
strStmt = 
  do reservedOp "\""
     chars <- many (noneOf "\"")
     reservedOp "\""
     return $ Val (Str chars)
  <|> do reservedOp "'"
         chars <- many (noneOf "'")
         reservedOp "'"
         return $ Val (Str chars)
     
intStmt :: Parser Expr
intStmt =
  do value <- integer
     whitespace <- whiteSpace
     return $ Val (Number (realToFrac value))
     
floatStmt :: Parser Expr
floatStmt =
  do value <- float
     whitespace <- whiteSpace
     return $ Val (Number value)

listStmt :: Parser Expr
listStmt =
  do reservedOp "["
     exprs <- exprList
     reservedOp "]"
     return $ Val (List exprs)
     
forStmt :: Parser Expr
forStmt =
  do reservedOp "for"
     iterator <- identifier
     reservedOp "in"
     list <- listStmt
     expr <- expression
     return $ For iterator list expr
     
defunStmt :: Parser Expr
defunStmt =
  do var <- identifier
     params <- parens idList
     reservedOp "="
     expr <- expression
     return $ Defun var params expr (Skip)
     
funcallStmt :: Parser Expr
funcallStmt =
  do var <- identifier
     params <- parens exprList
     return $ Func var params
 
skipStmt :: Parser Expr
skipStmt = reserved "skip" >> return Skip

operators :: [[ Operator Char st Expr ]]
operators = [[Infix  (reservedOp "^"   >> return (Exp             )) AssocLeft],
             [Infix  (reservedOp "*"   >> return (Prod            )) AssocLeft ,
              Infix  (reservedOp "/"   >> return (Div             )) AssocLeft],
             [Infix  (reservedOp "+"   >> return (Add             )) AssocLeft ,
              Infix  (reservedOp "-"   >> return (Sub             )) AssocLeft],
             [Infix  (reservedOp "=="  >> return (Eq              )) AssocLeft,
              Infix  (reservedOp ">"   >> return (Gt              )) AssocLeft,
              Infix  (reservedOp "<"   >> return (Lt              )) AssocLeft],
             [Infix  (reservedOp "and" >> return (And             )) AssocLeft,
              Infix  (reservedOp "or"  >> return (Or              )) AssocLeft,
              Infix  (reservedOp "&"   >> return (And             )) AssocLeft,
              Infix  (reservedOp "|"   >> return (Or              )) AssocLeft ]
             ]
             
statement :: Parser Expr
statement =  expression

expression :: Parser Expr
expression = try operation <|> term
           
operation = buildExpressionParser operators (try term <|> parens term)

term :: Parser Expr
term =   try syntax
     <|> parens expression
     
value = try listStmt
      <|> try strStmt
      <|> try floatStmt
      <|> try intStmt
     
syntax :: Parser Expr
syntax =  try (reserved "true" >> return (Val (Bit True)))
      <|> try (reserved "false" >> return (Val (Bit False)))
      <|> try (reserved "null" >> return (Val Null))
      <|> try defunStmt
      <|> try assignStmt
      <|> try ifStmt
      <|> try value
      <|> try skipStmt
      <|> try printStmt
      <|> try forStmt
      <|> try funcallStmt
      <|> liftM Var identifier

read s = case (parse parser "" s) of
            Right r -> r
            otherwise -> Undefined "Parse error"
