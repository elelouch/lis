module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Integer
lookfor var ((x,y):xs)= if var == x then y
                                    else lookfor var xs

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update var valor [] = [(var,valor)]
update var valor ((x,y):xs) = if var == x then (var,valor):xs
                                          else (x,y): update var valor xs

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip s = s
evalComm (Let var expInt) s = let n = evalIntExp expInt s
                                  in evalComm Skip (update var n s)
evalComm (Seq Skip c1) s = evalComm c1 s
evalComm (Seq c0 c1) s = let s' = evalComm c0 s
                                  in evalComm (Seq Skip c1) s'
evalComm (Cond b c0 c1) s = if (evalBoolExp b s )
                            then evalComm c0 s
                            else evalComm c1 s
evalComm (Repeat c b) s = evalComm (Seq c (Cond b Skip (Repeat c b))) s
-- Evalua una expresion entera
-- Completar definicion
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const valor) estado = valor
evalIntExp (Var variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = let valor = evalIntExp expInt estado
                                    in (-valor)
evalIntExp (Plus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                         valor2 = evalIntExp exp2 estado
                                         in valor1 + valor2

evalIntExp (Minus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                          in valor1 - valor2
evalIntExp (Times exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                          in valor1 * valor2
evalIntExp (Div exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in div valor1 valor2


-- Evalua una expresion entera
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue estado = True
evalBoolExp BFalse estado = False
evalBoolExp (Eq exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 == valor2

evalBoolExp (Lt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 < valor2

evalBoolExp (Gt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 > valor2

evalBoolExp (And exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                         valor2 = evalBoolExp exp2 estado
                                        in valor1 && valor2

evalBoolExp (Or exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                        valor2 = evalBoolExp exp2 estado
                                        in valor1 || valor2
evalBoolExp (Not exp1) estado = not (evalBoolExp exp1 estado)
