module Parser (parser) where
import AST
import Eval
import Control.Monad
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
                      "true","false", "for","procedure","invoke", "len",
                      "cons", "tail", "=","++","--"],
    reservedOpNames = ["+","-","*","/","==","<",">","and","or","not"],
    identStart = letter 
}

-- lexeme : unidad basica de significado
-- Reasigno las definiciones para no tener que usar Token ni tener problemas con definiciones
toInt :: Integer -> Int
toInt = fromIntegral
lexer = Token.makeTokenParser languageDefinition
parens = Token.parens lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
semi = Token.semi lexer
comma = Token.comma lexer
integer = liftM toInt (Token.integer lexer)
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
reserved = Token.reserved lexer
identifier = Token.identifier lexer

-- buildExpressionParser :: OperatorTable tok st a -> GenParser tok st a -> GenParser tok st a
-- Utilice la jerarquia expr -> term -> factor
intExp = buildExpressionParser intOps intTerm
boolExp = buildExpressionParser boolOps boolTerm
intOps = [ [(Infix (reservedOp "+" >> return Add) AssocLeft),
            (Infix (reservedOp "-" >> return Sub) AssocRight)],
           [(Infix (reservedOp "*" >> return Mult) AssocLeft),
             (Infix (reservedOp "/" >> return Div) AssocLeft)],
           [Prefix (reservedOp "-" >> return Neg)]]
boolOps = [ [(Infix (reservedOp "or" >> return Or) AssocLeft),
             (Infix (reservedOp "and" >> return And) AssocLeft)],
            [Prefix (reservedOp "not" >> return Not)]]

-- Promocionamos los constructores Var y Const a monadas para que realicen computaciones dentro. 
-- Evito usar do con liftM.
-- liftM f monad = do {result <- monad; return (f result)}

intTerm = parens intExp
       <|> liftM Const integer
       <|> liftM Len (reserved "len" >> parens listExp)
       <|> try (do l <- listExp
                   index <- brackets intExp
                   return (ListIndex index l))
       <|> try (liftM Var identifier)

boolTerm = parens boolExp
        <|> (reserved "true" >> return BTrue)
        <|> (reserved "false" >> return BFalse)
        <|> relTerm

-- Tuve que usar try para hacer backtracking 
relTerm = try (relTerm' (reservedOp "<") Lt) 
       <|> try (relTerm' (reservedOp ">") Gt)
       <|> try (relTerm' (reservedOp "==") Eq)

-- Parser general para relaciones
relTerm' p f = do 
    i1 <- intExp
    p 
    i2 <- intExp
    return $ f i1 i2

-- parser de procedures, lo utilizo con endBy al comienzo del parseo 
proc = do 
    reserved "procedure"
    procId <- identifier
    c <- braces comm
    return (procId,c)

comm = comm' `chainl1` (reservedOp ";" >> return Seq)

-- Separe los parsers de comandos por legibilidad
comm' = whileParser
     <|> ifParser
     <|> forParser
     <|> assignParser
     <|> liftM Invoke (reserved "invoke" >> identifier)
     <|> return Skip

assignParser = try (do 
    name <- identifier
    holder <- (holderIntExp <|> holderListExp)
    return (Assign name holder))
   <|> try (do name <- identifier
               op <- ((reserved "++" >> return Add) <|> (reserved "--" >> return Sub))
               return (Assign name (IntExpHolder (op (Var name) (Const 1)))))

holderIntExp = try (do
    reserved "="
    int <- intExp
    return (IntExpHolder int))

holderListExp = do
    reserved "<-"
    list <- listExp
    return (ListExpHolder list)

forParser = do 
    reserved "for"
    f <- forParens
    c <- braces comm
    return (f c)

forParens = parens (do 
    assign1 <- assignParser
    semi
    bExp <- boolExp
    semi
    assign2 <- assignParser
    return (For assign1 bExp assign2)) 

whileParser =  do 
    reserved "while"
    b <- parens boolExp
    c <- braces comm 
    return (While b c)

ifParser = do 
    reserved "if"
    b <- parens boolExp
    c1 <- braces comm 
    c2 <- (reserved "else" >> braces comm) <|> return Skip
    return (If b c1 c2)

-- Parseo nombre de la lista o lista constante o cons o tail
listExp = liftM ListVar identifier
       <|> liftM List (brackets $ intExp `sepBy` comma)
       <|> do reserved "cons" 
              parens (do 
                 i <- intExp
                 comma
                 l <- listExp
                 return $ Cons i l)
       <|> do reserved "tail"
              list <- parens listExp
              return (Tail list)

lisParser = do 
    s <- whiteSpace
    astList <- proc `endBy1` semi
    eof
    return astList

-- cada procedure es un AST
parser s = case parse lisParser "" s of 
               Left err -> error $ show err
               Right astList -> astList

-- interactive parser
parseFile file =
  do program  <- readFile file
     case parse lisParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return (eval r)
