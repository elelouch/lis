module AST where
type Variable = String

data IntExp = NVal Integer 
            | Var Variable
            | Neg IntExp
            | Sub IntExp IntExp
            | Add IntExp IntExp
            | Div IntExp IntExp
            | Mult IntExp IntExp deriving Show


data BoolExp = BTrue 
             | BFalse
             | Or BoolExp BoolExp
             | And BoolExp BoolExp
             | Not BoolExp
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp deriving Show

data Stmt = Skip
          | Seq Stmt Stmt
          | If BoolExp Stmt Stmt
          | While BoolExp Stmt
          | Assign Variable IntExp 
          | For BoolExp Stmt Stmt 
          | TempAssign Variable IntExp Stmt deriving Show
