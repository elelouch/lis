import AST
import Eval
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Char

languageDefinition = emptyDef { 
    commentStart = "/*",
    commentEnd = "*/",
    commentLine = "//",
    nestedComments = False,
    reservedNames = ["if","then","else","while","and","or","not", 
                      "do","done","true","false", "for", "temp"],
    reservedOpNames = ["+","-","*","/",":=","==","<",">"],
    identStart = letter 
}

-- lexeme : unidad basica de significado
lexer = Token.makeTokenParser languageDefinition

parens = Token.parens lexer
integer = Token.integer lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
reserved = Token.reserved lexer
identifier = Token.identifier lexer

cleanParse p = do whiteSpace
                  t <- p
                  eof
                  return t

intexp = expr `chainl1` addsubop

expr = term `chainl1` multdivop

term = parens intexp 
   <|> do reservedOp "-"
          t <- expr
          return $ Neg t
   <|> do i <- integer
          return $ NVal i
   <|> do varname <- identifier
          return $ Var varname

addsubop = do reservedOp "+" 
              return Add 
       <|> do reservedOp "-" 
              return Sub

multdivop = do reservedOp "*" 
               return Mult 
        <|> do reservedOp "/" 
               return Div

boolexp = boolterm `chainl1` (do reserved "and"
                                 return And)

boolterm = boolfact `chainl1` (do reserved "or"
                                  return Or)
boolfact = relop 
       <|> parens boolexp
       <|> do reserved "true"
              return BTrue
       <|> do reserved "false"
              return BFalse


relop = do iexp1 <- intexp
           reservedOp "<"
           iexp2 <- intexp
           return $ Lt iexp1 iexp2
    <|> do iexp1 <- intexp
           reservedOp ">"
           iexp2 <- intexp
           return $ Gt iexp1 iexp2
    <|> do iexp1 <- intexp
           reservedOp "=="
           iexp2 <- intexp
           return $ Eq iexp1 iexp2


-- relop = betweenIntExp (reservedOp ">") Lt
--     <|> betweenIntExp (reservedOp "==") Eq
--     <|> betweenIntExp (reservedOp "<") Gt
-- 
-- betweenIntExp parser constructor = do
--     iexp1 <- intexp
--     parser
--     iexp2 <- intexp
--     return $ constructor iexp1 iexp2

stmt = stmtexp `chainl1` (do reservedOp ";"
                             return Seq)
stmtexp = do reserved "if"
             b <- boolexp
             reserved "then"
             s1 <- stmt
             reserved "else"
             s2 <- stmt
             return $ If b s1 s2
      <|> do varid <- identifier
             reservedOp ":="
             iexp <- intexp
             return $ Assign varid iexp
      <|> do reserved "while"
             b <- parens boolexp
             reserved "do"
             s <- stmt
             reserved "done"
             return $ While b s
      <|> do reserved "for"
             forcons <- forhead
             reserved "do"
             body <- stmt
             reserved "done"
             return $ forcons body
      <|> do reserved "temp"
             tempCons <- parens (do varid <- identifier
                                    reservedOp ";"
                                    iexp <- intexp
                                    return $ TempAssign varid iexp)
             reserved "in"
             s <- stmt
             return $ tempCons s
      <|> return Skip

forhead = parens (do bexp <- boolexp
                     reservedOp ";"
                     s2 <- stmt
                     return $ For bexp s2)


parseFile file = 
    do program <- readFile file
       case (parse (cleanParse stmt) "" program) of
           Left e -> print e >> fail "parser error"
           Right ast -> return $ evalStmt ast env
