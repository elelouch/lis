module AST where
import Data.Data

type Variable = String

data ListExp = ListVar Variable 
             | List [IntExp] 
             | Cons IntExp ListExp
             | Tail ListExp deriving Show

data IntExp = Const Int
            | Var Variable
            | Neg IntExp
            | Sub IntExp IntExp
            | Add IntExp IntExp
            | Div IntExp IntExp
            | Len ListExp
            | ListIndex IntExp ListExp 
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
          | For Comm BoolExp Comm Comm 
          | Assign Variable ExpHolder
          | Invoke Variable 
          deriving Show

data ExpHolder = IntExpHolder IntExp | ListExpHolder ListExp deriving Show
