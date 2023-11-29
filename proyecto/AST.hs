module AST where
type Variable = String
type ProcEnv = [(Variable,Comm)]

data IntExp = Const Integer
            | Var Variable
            | Neg IntExp
            | Sub IntExp IntExp
            | Add IntExp IntExp
            | Div IntExp IntExp
            | Mult IntExp IntExp deriving Show

data ListExp = Cons IntExp ListExp | Empty

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
          | For IntExp BoolExp IntExp Comm deriving Show
