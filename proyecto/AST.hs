module AST where
type Variable = String
type ProcEnv = [(Variable,Comm)]

data Type = L List | I Integer deriving Eq

type List = [Integer]


data IntExp = Const Integer
            | Var Variable
            | Neg IntExp
            | Sub IntExp IntExp
            | Add IntExp IntExp
            | Div IntExp IntExp
            | LLen Variable
            | LVar Variable IntExp
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
          | For IntExp BoolExp IntExp Comm 
          | AssignList Variable List
          | SetAt IntExp IntExp Variable
          | Invoke Variable deriving Show

-- Para tail uso el parser
