import AST
-- import Eval
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
                      "true","false", "for","procedure","invoke", "len"],
    reservedOpNames = ["+","-","*","/","=","==","<",">","++","--","<=","and","or","not"],
    identStart = letter 
}

-- lexeme : unidad basica de significado
lexer = Token.makeTokenParser languageDefinition
parens = Token.parens lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
semi = Token.semi lexer
comma = Token.comma lexer
integer = Token.integer lexer
reservedOp = Token.reservedOp lexer
whiteSpace = Token.whiteSpace lexer
reserved = Token.reserved lexer
identifier = Token.identifier lexer

procEnv = []
-- Parsers de expresiones tranquis

-- buildExpressionParser :: OperatorTable tok st a -> GenParser tok st a -> GenParser tok st a
-- Se recurren a los terminos para parsear entre operadores, como siempre.
intExp = buildExpressionParser intOps intTerm
boolExp = buildExpressionParser boolOps boolTerm
--              parser que retorne el operador
intOps = [ [(Infix (reservedOp "+" >> return Add) AssocLeft),
            (Infix (reservedOp "-" >> return Sub) AssocLeft)],
           [(Infix (reservedOp "*" >> return Mult) AssocLeft),
             (Infix (reservedOp "/" >> return Div) AssocLeft)],
           [Prefix (reservedOp "-" >> return Neg)]]
-- agruparlos en listas de un poco lo mismo, podria ponerlos todos en la misma
boolOps = [ [(Infix (reservedOp "or" >> return Or) AssocLeft),
             (Infix (reservedOp "and" >> return And) AssocLeft)],
            [Prefix (reservedOp "not" >> return Not)]]

-- Promocionamos los constructores Var y Const a monadas, con identifier e integer
-- para que realicen computaciones dentro. Puedo reemplazar algunos do's.
-- liftM f monad = do {result <- monad; return (f result)}
intTerm = parens intExp
       <|> liftM Var identifier -- Var (identifier)
       <|> liftM Const integer
       <|> do listId <- identifier
              index <- brackets intExp
              return $ LVar listId index
       <|> liftM LLen (do {reserved "len"; parens identifier})
              

boolTerm = parens boolExp
        <|> (reserved "true" >> return BTrue)
        <|> (reserved "false" >> return BFalse)
        <|> relTerm

relTerm = try (relTerm' (reservedOp "<") Lt) 
       <|> try (relTerm' (reservedOp ">") Gt)
       <|> try (relTerm' (reservedOp "==") Eq)

relTerm' p f = do i1 <- intExp
                  p 
                  i2 <- intExp
                  return $ f i1 i2

proc = do reserved "procedure"
          procId <- identifier
          c <- braces comm
          return (procId,c)

comm = liftM Seq (comm' `endBy1`  semi)

comm' = do reserved "while"
           b <- parens boolExp
           c <- braces comm 
           return $ While b c
     <|> do reserved "if"
            b <- parens boolExp
            c1 <- braces comm 
            c2 <- (reserved "else" >> braces comm) <|> return Skip
            return $ If b c1 c2
     <|> try (do id <- identifier
                 reservedOp "="
                 i <- intExp
                 return $ Assign id i)
     <|> try (do id <- identifier
                 reservedOp "<="
                 list <- brackets $ integer `sepBy` comma
                 return $ AssignList id list)
     <|> try (do listId <- identifier
                 i <- brackets intExp
                 reservedOp "="
                 newVal <- intExp
                 return $ SetAt i newVal listId)
     <|> return Skip

lisParser = do s <- whiteSpace
               ast <- proc `endBy1` semi
               eof
               return ast

parseAux str = case parse lisParser "" str of
               Left err -> error $ show err
               Right ast -> return ast

parseFile file = do program <- readFile file
                    parseAux program


-- Para implementar funciones :
-- En este parser, cada vez que encuentro funciones con procedures,  
-- lo unico que tengo que hacer es guardarlas en una lista de pares 
-- [(IdFuncion, Comm)]
-- Y pasarlo al evaluador
