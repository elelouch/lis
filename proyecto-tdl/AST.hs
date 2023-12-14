module AST where
import Data.Data

type Variable = String
--
-- ya no puedo parsear listas con integer, tengo que usar intExp
type IntExpList = [IntExp]

data ListExp = ListVar Variable 
             | List IntExpList 
             | Cons IntExp ListExp
             | Tail ListExp deriving Show

-- LVar y Var van a terminar devolviendo algo del env
data IntExp = Const Int
            | Var Variable
            | Assign Variable IntExp 
            | Neg IntExp
            | Sub IntExp IntExp
            | Add IntExp IntExp
            | Div IntExp IntExp
            | Len ListExp
            | ListAt IntExp ListExp 
            | Mult IntExp IntExp 
            deriving Show

data BoolExp = BTrue 
             | BFalse
             | Or BoolExp BoolExp
             | And BoolExp BoolExp
             | Not BoolExp
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp 
             deriving Show

data Comm = Skip
          | Seq Comm Comm
          | If BoolExp Comm Comm
          | While BoolExp Comm
          | For IntExp BoolExp IntExp Comm 
          | AssignVar Variable IntExp
          | AssignList Variable ListExp
          | Invoke Variable 
          deriving Show

-- Puedo hacer por debajo del parser las asignaciones, o por debajo de un
-- AssignInt que devuelva eso
