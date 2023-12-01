module AST where
import Data.Data

type Variable = String
--
-- ya no puedo parsear listas con integer, tengo que usar intExp
type IntExpList = [IntExp]

-- Como parseo listas y entero a la vez en assign
-- exp?
data ListExp = LVar Variable 
             | LConst IntExpList 
             | LCons IntExp ListExp
             | LTail ListExp deriving Show

-- LVar y Var van a terminar devolviendo algo del env
data IntExp = Const Int
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

data Comm = Skip
          | Seq [Comm]
          | If BoolExp Comm Comm
          | While BoolExp Comm
          | Assign Variable IntExp 
          | LAssign Variable ListExp
          | For Comm BoolExp Comm Comm 
          | Invoke Variable deriving Show

-- El for podria tener IntExp, tipo
-- For IntExp BoolExp IntExp Comm
-- Para esto, las asignaciones deberian poder evaluar al entero asignado
-- x = 3 evalua a 3
