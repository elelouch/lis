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
                      "cons", "tail"],
    reservedOpNames = ["+","-","*","/","=","==","<",">","++","--","and","or","not"],
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
       <|> try (do l <- listExp
                   index <- brackets intExp
                   return $ LVal index l)
       <|> liftM Var identifier -- Var (identifier)
       <|> liftM LLen (reserved "len" >> parens listExp)    

boolTerm = parens boolExp
        <|> (reserved "true" >> return BTrue)
        <|> (reserved "false" >> return BFalse)
        <|> relTerm

-- Tuve que usar try para hacer backtracking 
relTerm = try (relTerm' (reservedOp "<") Lt) 
       <|> try (relTerm' (reservedOp ">") Gt)
       <|> try (relTerm' (reservedOp "==") Eq)

-- Parser general para relaciones
relTerm' p f = do i1 <- intExp
                  p 
                  i2 <- intExp
                  return $ f i1 i2

-- parser de procedures, lo utilizo con endBy al comienzo del parseo 
proc = do reserved "procedure"
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

-- Parseo nombre de la lista o lista constante o cons o tail
listExp = liftM LVar identifier
       <|> liftM LConst (brackets $ intExp `sepBy` comma)
       <|> do reserved "cons" 
              parens (do i <- intExp
                         comma
                         l <- listExp
                         return $ LCons i l)
       <|> do reserved "tail"
              l <- parens listExp
              return $ LTail l

-- parser para asignar
assignParser = try (do varname <- identifier
                       reservedOp "="
                       i <- intExp
                       return $ Assign varname i)
            <|> try (do varname <- identifier
                        reservedOp "<-"
                        list <- listExp
                        return $ LAssign varname list)
            <|> do varname <- identifier
                   reservedOp "++"
                   return (Assign varname (Add (Var varname) (Const 1)))
            <|> do varname <- identifier
                   reservedOp "--"
                   return (Assign varname (Sub (Var varname) (Const 1)))

forParser = do reserved "for"
               f <- forParensParser
               c <- braces comm
               return (f c)

forParensParser = parens (do a1 <- assignParser
                             semi
                             b <- boolExp
                             semi
                             a2 <- assignParser
                             return $ For a1 b a2) 

whileParser =  do reserved "while"
                  b <- parens boolExp
                  c <- braces comm 
                  return $ While b c

ifParser = do reserved "if"
              b <- parens boolExp
              c1 <- braces comm 
              c2 <- (reserved "else" >> braces comm) <|> return Skip
              return $ If b c1 c2

lisParser = do s <- whiteSpace
               ast <- proc `endBy1` semi
               eof
               return ast
-- cada vez que encuentro procedures, las guardo en una lista como pares junto 
-- a su nombre

parser s = case parse lisParser "" s of 
               Left err -> print err >> fail "parser error"
               Right listProcs -> return listProcs

