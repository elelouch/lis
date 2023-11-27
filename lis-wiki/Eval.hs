module Eval where

------------------ CON MANEJO DE ERRORES --------------------------------------

import AST
-- type Env = [(Variable,Integer)]
type Env = Either Error [(Variable,Integer)]
data Error = DivByZero | VarNotFound deriving Show

env = []

lookfor name [] = Left VarNotFound
lookfor name ((varname,val) : vs) = 
    if name == varname 
        then Right val
        else lookfor name vs

update name val [] = [(name,val)]
update name newval ((varname,val):vs) = 
    if name == varname
        then ((name,newval):vs)
        else (varname,val) : update name newval vs

evalIntExp (NVal x) s = Right x
evalIntExp (Var name) s = lookfor name s
evalIntExp (Neg x) s = 
    case evalIntExp x s of
        Left err -> Left err
        Right val -> Right $ (- val)
evalIntExp (Add x y) s = evalValues (evalIntExp x s) (evalIntExp y s) (+)
evalIntExp (Sub x y) s = evalValues (evalIntExp x s) (evalIntExp y s) (-)
evalIntExp (Mult x y) s = evalValues (evalIntExp x s) (evalIntExp y s) (*)
evalIntExp (Div x y) s = 
    let divisorAux = evalIntExp y s
    in case divisorAux of
        Left err -> Left err
        Right divisor -> if divisor == 0 
                            then Left DivByZero
                            else evalValues (evalIntExp x s) divisorAux (div)
evalBoolExp (BTrue) s = Right True
evalBoolExp (BFalse) s = Right False
evalBoolExp (Or a b) s = evalValues (evalBoolExp a s) (evalBoolExp b s) (||)
evalBoolExp (And a b) s = evalValues (evalBoolExp a s) (evalBoolExp b s) (&&)
evalBoolExp (Lt a b) s = evalValues (evalIntExp a s) (evalIntExp b s) (<)
evalBoolExp (Gt a b) s = evalValues (evalIntExp a s) (evalIntExp b s) (>)
evalBoolExp (Eq a b) s = evalValues (evalIntExp a s) (evalIntExp b s) (==)

evalValues (Left err) _ _ = Left err
evalValues _ (Left err) _ = Left err
evalValues (Right x) (Right y) op = Right $ op x y

evalStmt Skip env = Right env
evalStmt (Assign name val) env = 
    let res = evalIntExp val env
    in case res of
        Left err -> Left err
        Right result -> Right $ update name result env
evalStmt (Seq stmt1 stmt2) env = 
    let newState = evalStmt stmt1 env
    in case newState of
        Left err -> Left err
        Right state -> evalStmt stmt2 state

evalStmt (While b s) env = evalStmt (If b (Seq s (While b s)) Skip) env

evalStmt (If b s1 s2) env = 
    let boolResAux = evalBoolExp b env
    in case boolResAux of
           Left err -> Left err
           Right boolRes -> evalStmt (if boolRes then s1 else s2) env

------------------- SIN MANEJO DE ERRORES ------------------------------

--lookfor name ((varname, val):vs) = 
--    if name == varname 
--        then val
--        else lookfor name vs
--
--update name newVal [] = [(name,newVal)]
--update name newVal ((varName,val):vs) = 
--    if name == varName 
--        then (name,newVal) : vs
--        else (varName,val) : update name newVal vs
--
--evalIntExp (NVal a) env = a
--evalIntExp (Var name) env = lookfor name env 
--evalIntExp (Add exp1 exp2) env = evalIntExp exp1 env + evalIntExp exp2 env
--evalIntExp (Sub exp1 exp2) env = evalIntExp exp1 env - evalIntExp exp2 env
--evalIntExp (Mult exp1 exp2) env = evalIntExp exp1 env * evalIntExp exp2 env
--evalIntExp (Div exp1 exp2) env = evalIntExp exp1 env `div` evalIntExp exp2 env
--evalIntExp (Neg exp) env = -(evalIntExp exp env)
--
--evalBoolExp (BTrue) env = True
--evalBoolExp (BFalse) env = False
--evalBoolExp (Not bexp) env = not $ evalBoolExp bexp env
--evalBoolExp (Lt iexp1 iexp2) env = evalIntExp iexp1 env < evalIntExp iexp2 env
--evalBoolExp (Gt iexp1 iexp2) env = evalIntExp iexp1 env > evalIntExp iexp2 env
--evalBoolExp (Eq iexp1 iexp2) env = evalIntExp iexp1 env == evalIntExp iexp2 env
--evalBoolExp (Or bexp1 bexp2) env = evalBoolExp bexp1 env || evalBoolExp bexp2 env
--evalBoolExp (And bexp1 bexp2) env = evalBoolExp bexp1 env && evalBoolExp bexp2 env
--
--evalStmt Skip env = env
--evalStmt (If bexp s1 s2) env = evalStmt (if evalBoolExp bexp env then s1 else s2) env
--evalStmt (While bexp s) env =  
--    if evalBoolExp bexp env
--        then let newState = evalStmt s env
--             in evalStmt (While bexp s) newState
--        else evalStmt Skip env
--evalStmt (Assign name val) env = update name (evalIntExp val env) env
--evalStmt (Seq s1 s2) env = 
--    let newState = evalStmt s1 env
--    in evalStmt s2 newState
