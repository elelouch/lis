module AST where
import Data.Data

type Variable = String

-- utilizar toConstr para tomar los constructores y compararlos en el eval

-- ya no puedo parsear listas con integer, tengo que usar intExp
type IntExpList = [IntExp]

-- Como parseo listas y entero a la vez en assign
-- exp?
data ListExp = LVar Variable 
             | LConst IntExpList 
             | LCons IntExp ListExp
             | LTail ListExp deriving Show

-- LVar y Var van a terminar devolviendo algo del env
data IntExp = Const Integer
            | Var Variable
            | Neg IntExp
            | Sub IntExp IntExp
            | Add IntExp IntExp
            | Div IntExp IntExp
            | LLen ListExp
            | LVal IntExp ListExp 
            | Mult IntExp IntExp deriving Show

data BoolExp = BTrue 
             | BFalse
             | Or BoolExp BoolExp
             | And BoolExp BoolExp
             | Not BoolExp
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp deriving Show

-- Considerar manejar clases como data Error = TypeError | NotFound | DivByZero
-- hacer un update que no agregue al final y tal vez tire Nothing
-- hacer un add que agregue al principio

data VarType = IExp IntExp | List ListExp deriving Show
data Comm = Skip
          | Seq [Comm]
          | If BoolExp Comm Comm
          | While BoolExp Comm
          | Assign Variable VarType 
          | For Comm BoolExp Comm Comm 
          | Invoke Variable deriving Show
