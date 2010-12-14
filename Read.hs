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
             Token.reservedNames   = ["if", "then", "else",
                                      "for", "in",
                                      "print", "skip",
                                      "true", "false",
                                      "and", "or", "not"
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
parser = whiteSpace >> expression

-- expression parsers

expression :: Parser Expr
expression = try operation <|> term
           
operation = buildExpressionParser operators (try term <|> parens term)

term :: Parser Expr
term = try syntax <|>
       parens expression
     
value = try listValue <|> try strValue <|> try floatValue <|> try intValue
valueStmt = try listStmt <|>
            try strStmt <|>
            try floatStmt <|>
            try intStmt
     
syntax :: Parser Expr
syntax = try (reserved "true" >> return (Val (Bit True))) <|>
         try (reserved "false" >> return (Val (Bit False))) <|>
         try (reserved "null" >> return (Val Null)) <|>
         try defunStmt <|>
         try eagerStmt <|>
         try assignStmt <|>
         try ifStmt <|>
         try skipStmt <|>
         try printStmt <|>
         try forStmt <|>
         try valueStmt <|>
         try funcallStmt <|>
         try varcallStmt

-- syntax parsers

defunStmt :: Parser Expr
defunStmt =
  do var <- identifier
     params <- parens idList
     reservedOp "="
     expr <- expression
     return $ Defun (Name var) params expr Placeholder

eagerStmt :: Parser Expr
eagerStmt =
  do var <- identifier
     w <- whiteSpace
     reservedOp ":="
     w <- whiteSpace
     expr1 <- expression
     return $ EagerDef (Name var) expr1 Placeholder
     
assignStmt :: Parser Expr
assignStmt =
  do var <- identifier
     w <- whiteSpace
     reservedOp "="
     w <- whiteSpace
     expr1 <- expression
     return $ Def (Name var) expr1 Placeholder
           
ifStmt :: Parser Expr
ifStmt =
  do reserved "if"
     cond  <- expression
     reserved "then"
     stmt1 <- expression
     reserved "else"
     stmt2 <- expression
     return $ If cond stmt1 Placeholder
     
skipStmt :: Parser Expr
skipStmt = reserved "skip" >> return Skip

printStmt :: Parser Expr
printStmt =
  do reservedOp "print"
     expr <- expression
     return $ Output (expr) Placeholder
     
forStmt :: Parser Expr
forStmt =
  do reservedOp "for"
     iterator <- identifier
     reservedOp "in"
     list <- expression
     expr <- expression
     return $ For (Name iterator) list expr

-- value parsers

exprList :: Parser [Expr]
exprList = sepBy expression (oneOf ",")

identifierOrValue :: Parser Id
identifierOrValue = 
    do id <- identifier
       return $ Name id 
    <|> 
    do val <- value
       return $ Pattern val

idList :: Parser [Id]
idList = do id <- sepBy identifierOrValue (oneOf ",")
            return $ id

strValue :: Parser Value
strValue = 
  do reservedOp "\""
     chars <- many (noneOf "\"")
     reservedOp "\""
     return $ Str chars
  <|> do reservedOp "'"
         chars <- many (noneOf "'")
         reservedOp "'"
         return $ Str chars
strStmt :: Parser Expr
strStmt =
  do str <- strValue
     return $ Val str
     
intValue :: Parser Value
intValue =
  do value <- integer
     whitespace <- whiteSpace
     return $ Number (realToFrac value)
intStmt :: Parser Expr
intStmt =
  do value <- intValue
     return $ Val value
     
floatValue :: Parser Value
floatValue =
  do value <- float
     whitespace <- whiteSpace
     return $ Number value
floatStmt :: Parser Expr
floatStmt =
  do value <- floatValue
     return $ Val value

listValue :: Parser Value
listValue =
  do reservedOp "["
     exprs <- exprList
     reservedOp "]"
     return $ List exprs
listStmt :: Parser Expr
listStmt =
  do list <- listValue
     return $ Val list
     
     
funcallStmt :: Parser Expr
funcallStmt =
  do var <- identifier
     params <- parens exprList
     return $ Func (Name var) params
     
varcallStmt :: Parser Expr
varcallStmt =
  do var <- identifier
     return $ Var (Name var)

-- operator table

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

read s = case (parse parser "" s) of
            Right r -> r
            otherwise -> Undefined "Parse error"
