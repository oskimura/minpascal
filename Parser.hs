module Main where
import AS

import System.IO
import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)




simple :: Parser Char
simple = letter

{--
pascal_program
    =　program identifier program_headingopt
    | block

program_headingopt
    = identifier_list

identifier_list
    = identifier
    | identifier_list , identifier



block
    = block1
    | labels_declaration
    | block1

block1
    = block2
    | constant_declaration
    | block2


block2 = block3
       | type_declaration
       | block3

--}



program
    = do { whiteSpace
         ; e <- expr
         ; return e
         }

decs
    = many dec

dec
    = tydec
      <|> vardec
      <|> fundec


tydec :: Parser Declaration
tydec
    = do { reserved "type"
         ; tid <- identifier
         ; symbol "="
         ; t <- ty
         ; return (TypeDec tid t)
         }

ty = do {fields <- braces tyfields ; return (Record fields) }
     <|>
     do
       { reserved "array"
       ; reserved "of"
       ; tid <- identifier
       ; return (Array tid)
       }
     <|>
     do { id <- identifier
        ; return (Var id)
        }

tyfields = commaSep field

noType = "*"
voidType = "void"


field =
    do { id  <- identifier
       ; symbol ":"
       ; tid <- identifier
       ; return (TypeVar id tid)
       }

vardec =
    do { reserved "var"
       ; id <- identifier
       ; t  <- option noType (try (do{symbol ":" ; identifier }))
       ; symbol ":="
       ; e  <- expr
       ; return (VarDec id t e)
       }

fundec =
    do { reserved "function"
       ; name    <- identifier
       ; parms   <- parens tyfields
       ; rettype <- option voidType (do{ symbol ":" ; identifier })
       ; symbol "="
       ; body    <- expr
       ; return (FunDec name parms rettype body)
       }


lfact :: Parser a -> Parser (a -> a) -> Parser a
lfact p q =
    do { a  <- p
       ; fs <- many q
       ; return (foldl (\x f -> f x) a fs)
       }


lvalue = lfact variable (recordref <|> subscripted)

recordref =
    do { symbol "."
       ; id <- variable
       ; return (\x -> Dot x id)
       }


subscripted =
    do {indexexpr <- brackets expr
       ; return (\x -> Sub x indexexpr)
       }


exprs = many expr

expr :: Parser Expr
expr = choice
       [ do {reserved "break"
             ; return Break
             }

       , ifExpr
       , whileExpr
       , letExpr
       , sequenceExpr
       , infixExpr
       ]

recordExpr :: Parser Expr
recordExpr = do { tid <- identifier
                ; symbol "{"
                ; fields <- commaSep1 fieldAssign
                ; symbol "}"
                ; return (RecordVal tid fields)
                }

fieldAssign :: Parser AssignField
fieldAssign =
    do { id <- identifier
       ; symbol "="
       ; e <- expr
       ; return (AssignField id e)
       }
arrayExpr :: Parser Expr
arrayExpr =
    do { tid <- identifier
       ; size <- brackets expr
       ; reserved "of"
       ; initvalue <- expr
       ; return (ArrayVal tid size initvalue)
       }
assignExpr :: Parser Expr
assignExpr = do {lv <- lvalue
                ; symbol ":="
                ; e <- expr
                ; return (Assign lv e)
                }
ifExpr :: Parser Expr
ifExpr =
    do { reserved "if"
       ; cond <- expr
       ; reserved "then"
       ; thenpart <- expr
       ; elsepart <- option Skip (do{reserved "else"; expr})
       ; return (If cond thenpart elsepart)
       }

whileExpr :: Parser Expr
whileExpr =
    do { reserved "while"
       ; cond <- expr
       ; reserved "do"
       ; body <- expr
       ; return (While cond body)
       }
forExpr :: Parser Expr
forExpr =
    do { reserved "for"
       ; id <- identifier
       ; symbol ":="
       ; lowerbound <- expr
       ; reserved "to"
       ; upperbound <- expr
       ; symbol "do"
       ; body <- expr
       ; return (For id lowerbound upperbound body)
       }

letExpr :: Parser Expr
letExpr =
    do { reserved "let"
       ; ds <- decs
       ; reserved "in"
       ; es <- semiSep expr
       ; reserved "end"
       ; return (Let ds es)
       }

sequenceExpr :: Parser Expr
sequenceExpr =
    do { exps <- parens (semiSep1 expr)
       ; return (if length exps < 2 then　head exps else Seq exps)
       }
infixExpr :: Parser Expr
infixExpr = buildExpressionParser operators simpleExpr

operators =
    [ [ prefix "-" ]
    , [ op "*"  AssocLeft, op "/" AssocLeft ]
    , [ op "+"  AssocLeft, op "-" AssocLeft ]
    , [ op "="  AssocNone, op "<>" AssocNone, op "<=" AssocNone
      , op "<"  AssocNone, op ">=" AssocNone, op ">" AssocNone ]
    , [ op "&"  AssocRight ]
    , [ op "|"  AssocRight ]
    , [ op ":=" AssocRight ]
    ]
    where
      op name assoc =
          Infix (do {reservedOp name
                    ; return (\x y -> Op name x y)
                    }) assoc
      prefix name  =
          Prefix (do{ reservedOp name
                    ; return (\x -> UnOp name x)
                    })


simpleExpr =
    choice [ do { reserved "nil"
                ; return Nil
                }
           , intLiteral
           , strLiteral
           , parens expr
           , try funCallExpr
           , try recordExpr
           , try arrayExpr
           , lvalue
           ]

funCallExpr =
    do { id    <- identifier
       ; parms <- parens (commaSep expr)
       ; return (Apply id parms)
       }


intLiteral =
    do { i <- integer; return (IntLit i) }
strLiteral =
    do { s <- stringLiteral; return (StringLit s) }
variable =
    do { id <- identifier
       ; return (Ident id)
       }


lexer = P.makeTokenParser tigerDef
tigerDef = javaStyle
           {
             P.reservedNames = [ "array", "break","do","else","end","for","function",
                                 "if","in","let"
                               ,"nil","of","then","to","type","var","while"]
             , P.reservedOpNames = ["<", "<=", ">", ">=", ":=", "+", "&", "-", "/"]
             , P.opLetter        = oneOf (concat (P.reservedOpNames tigerDef))
             , P.caseSensitive   = True
           }


parens          = P.parens lexer
braces          = P.braces lexer
semiSep         = P.semiSep lexer
semiSep1        = P.semiSep1 lexer
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
whiteSpace      = P.whiteSpace lexer
symbol          = P.symbol lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer
charLiteral     = P.charLiteral lexer
stringLiteral   = P.stringLiteral lexer



run :: Show a =>  Parser a -> String -> IO ()
run p input
    = case (parse p "" input) of
        Left err -> do { putStr "parser error at"
                       ; print err
                       }
        Right x -> print x

main :: IO ()
main =   getArgs >>= (putStr . head)

