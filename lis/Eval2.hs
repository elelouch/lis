module Eval (eval) where

import AST

-- Estados
data Error = UndefVar | DivByZero deriving (Show,Eq)
type State = Either Error [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = Right []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Error Integer
lookfor v (Right s) = lookfor' v s

lookfor' :: Variable -> [(Variable,Integer)] -> Either Error Integer
lookfor' var [] = Left UndefVar
lookfor' var ((x,y):xs)= if var == x then Right y
                                    else lookfor' var xs

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> [(Variable,Integer)] -> [(Variable,Integer)]
update var valor [] = [(var,valor)]
update var valor ((x,y):xs) = if var == x then (var,valor):xs
                                          else (x,y): update var valor xs

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

unR (Right x) = x
-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip e = e
evalComm (Let var expInt) estado = let valor = evalIntExp expInt estado
                                       in case valor of
                                               Right v -> Right (update var v (unR estado))
                                               Left er -> Left er
evalComm (Seq Skip c1) s = evalComm c1 s
evalComm (Seq c1 c2) s = let s' = evalComm c1 s
                                  in evalComm (Seq Skip c2) s'
evalComm (Cond b c1 c2) s = case (evalBoolExp b s) of
                                    Right True -> evalComm c1 s
                                    Right False -> evalComm c2 s
                                    Left er -> Left er
evalComm (Repeat c b) s = evalComm (Seq c (Cond b Skip (Repeat c b))) s
-- Evalua una expresion entera
-- Completar definicion
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const valor) estado = Right valor
evalIntExp (Var variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = let valor = evalIntExp expInt estado
                                    in case valor of
                                            Right x1 -> Right (-x1)
evalIntExp (Plus exp1 exp2) estado = propagar exp1 exp2 (+) estado
evalIntExp (Minus exp1 exp2) estado = propagar exp1 exp2 (-) estado
evalIntExp (Times exp1 exp2) estado = propagar exp1 exp2 (*) estado

evalIntExp (Div exp1 exp2) estado = case (evalIntExp exp1 estado) of
                                           Right x1 -> case (evalIntExp exp2 estado) of
                                                               Right 0  -> Left DivByZero
                                                               Right x2 -> Right (div x1 x2)
                                                               Left er -> Left er
                                           Left er -> Left er

propagar e1 e2 op s =  let valor1 = evalIntExp e1 s
                           valor2 = evalIntExp e2 s
                                          in case valor1 of
                                                  Right x1 -> case valor2 of
                                                                   Right x2 -> Right ((op) x1 x2)
                                                                   Left er -> Left er
                                                  Left er -> Left er

propagarB e1 e2 op s =  let valor1 = evalBoolExp e1 s
                            valor2 = evalBoolExp e2 s
                                          in case valor1 of
                                                  Right x1 -> case valor2 of
                                                                   Right x2 -> Right ((op) x1 x2)
                                                                   Left er -> Left er
                                                  Left er -> Left er


-- Evalua una expresion entera
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue estado = Right True
evalBoolExp BFalse estado = Right False
evalBoolExp (Eq exp1 exp2) estado = propagar exp1 exp2 (==) estado
evalBoolExp (Lt exp1 exp2) estado = propagar exp1 exp2 (<) estado
evalBoolExp (Gt exp1 exp2) estado = propagar exp1 exp2 (>) estado
evalBoolExp (And exp1 exp2) estado = propagarB exp1 exp2 (&&) estado
evalBoolExp (Or exp1 exp2) estado = propagarB exp1 exp2 (||) estado
evalBoolExp (Not exp1) estado = case (evalBoolExp exp1 estado) of
                                      Right x1 -> Right (not x1)
                                      Left er -> Left er

