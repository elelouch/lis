module Eval(eval,getInvokes,getState,getAll,getInvokesNState) where
import AST
import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)
import Data.Data


type IntList = [Int]
type Env = ([(Variable,Int)],[(Variable,IntList)])
type ProcEnv = [(Variable,Comm)]

data SaveState a = S a
                  | DivByZero 
                  | ProcedureNotFound 
                  | TailEmptyList 
                  | IndexOutOfBounds
                  | VarNotFound deriving Show

getState (S (_,_,state,_)) = state
getInvokes (S (_,_,_,invokes)) = invokes
getAll (S a) = a
getInvokesNState (S (_,_,state,invokes)) = (state,invokes)

newtype StateErrorProcs a = StateErrorProcs { 
    runStateErrorProcs :: ProcEnv -> Env -> SaveState (a,ProcEnv,Env,Int)
} 

class Monad m => MonadProcs m where 
   lookforProc :: Variable -> m Comm

class Monad m => MonadState m where 
    updateInt :: Variable -> Int -> m ()
    updateList :: Variable -> IntList -> m ()
    lookforInt :: Variable -> m Int
    lookforList :: Variable -> m IntList


class Monad m => MonadTick m where
    tick :: m ()

class Monad m => MonadError m where 
    throwDivByZero :: m a
    throwVarNotFound :: m a
    throwProcedureNotFound :: m a
    throwTailEmptyList :: m a
    throwIndexOutOfBounds :: m a
    throwMainProcedureNotFound :: m a

-- Recordar que esta monada guarda la ultima operacion realizada
instance Monad StateErrorProcs where 
    return x = StateErrorProcs (\proceds env -> S (x,proceds,env,0))
    (>>=) m f = StateErrorProcs (\proceds env -> do 
                            (lastComp,ps,e,invokes) <- (runStateErrorProcs m) proceds env
                            (newComp,ps',e',invokes') <- (runStateErrorProcs (f lastComp)) ps e
                            return $ (newComp,ps',e',invokes + invokes'))

lookfor _ [] = Nothing
lookfor name ((varname,val):vs) = 
    if varname == name 
        then Just val 
        else lookfor name vs
update name nval [] = [(name,nval)]
update name nval ((varname,val):vs) = 
    if name == varname 
        then ((name,nval):vs) 
        else (varname,val) : update name nval vs

instance MonadProcs StateErrorProcs where 
    lookforProc name =
       StateErrorProcs (\proceds env -> maybe ProcedureNotFound (\c -> S (c,proceds,env,0)) (lookfor name proceds))

instance MonadState StateErrorProcs where 
    lookforInt name =
        StateErrorProcs (\proceds env -> maybe VarNotFound (\v -> S (v,proceds,env,0)) (lookfor name (fst env)))
    lookforList name =
        StateErrorProcs (\proceds env -> maybe VarNotFound (\v -> S (v,proceds,env,0)) (lookfor name (snd env)))
    updateInt name val = 
        StateErrorProcs (\proceds env -> S ((),proceds, (update name val (fst env), snd env), 0))
    updateList name val = 
        StateErrorProcs (\proceds env -> S ((),proceds, (fst env,update name val (snd env)), 0))

instance MonadTick StateErrorProcs where 
    tick = StateErrorProcs (\proceds env -> S ((), proceds, env, 1))

instance Monad SaveState where 
    return x = S x
    (>>=) m f = case m of 
                    S x -> f x
                    DivByZero -> DivByZero
                    ProcedureNotFound -> ProcedureNotFound  
                    VarNotFound -> VarNotFound  
                    TailEmptyList -> TailEmptyList  
                    IndexOutOfBounds -> IndexOutOfBounds

instance Applicative SaveState where
    pure = return
    (<*>) = ap

instance Functor SaveState where 
    fmap = liftM

instance Applicative StateErrorProcs where
    pure = return
    (<*>) = ap

instance Functor StateErrorProcs where 
    fmap = liftM

instance MonadError StateErrorProcs where
    throwDivByZero = StateErrorProcs(\_ _ -> DivByZero)
    throwVarNotFound = StateErrorProcs(\_ _-> VarNotFound)
    throwProcedureNotFound = StateErrorProcs(\_ _-> ProcedureNotFound)
    throwIndexOutOfBounds = StateErrorProcs(\_ _-> IndexOutOfBounds)
    throwTailEmptyList = StateErrorProcs(\_ _-> TailEmptyList)

eval procenv = 
    case lookfor "main" procenv of
        Nothing -> ProcedureNotFound
        Just mainProc -> (runStateErrorProcs (evalComm mainProc)) procenv ([],[])

evalComm Skip = return ()
evalComm (Seq c1 c2) = evalComm c1 >> evalComm c2
evalComm (Assign var i) = do i' <- evalIntExp i
                             updateInt var i'
evalComm (If b c1 c2) = do b' <- evalBoolExp b
                           evalComm (if b' then c1 else c2)

evalComm (For c1 b c2 body) = evalComm (Seq c1 (While b (Seq body c2)))
evalComm (While b c) = evalComm (If b (Seq c (While b c)) Skip)

evalComm (LAssign var l) = do l' <- evalListExp l
                              updateList var l'

evalComm (Invoke name) = do c <- lookforProc name
                            tick
                            evalComm c   

evalListExp (LVar name) = lookforList name
evalListExp (LConst []) = return []
evalListExp (LConst (i:is)) = do x <- evalIntExp i
                                 xs <- evalListExp (LConst is)
                                 return (x:xs)
evalListExp (LCons val l) = do vs <- evalListExp l
                               v <- evalIntExp val
                               return (v:vs)
evalListExp (LTail list) = do l <- evalListExp list
                              if null l
                                  then throwTailEmptyList
                                  else return (tail l)

evalIntExp (Const n) = return n
evalIntExp (LVal i l) = do i' <- evalIntExp i
                           l' <- evalListExp l
                           if (i' >= length l' || i' < 0)
                               then throwIndexOutOfBounds
                               else return (l' !! i')
evalIntExp (LLen l) = do l' <- evalListExp l
                         return $ length l'
evalIntExp (Var name) = lookforInt name
evalIntExp (Neg v) = do val <- evalIntExp v
                        return (negate val)
evalIntExp (Add l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r 
                          return (lval + rval)
evalIntExp (Sub l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r 
                          return (lval - rval)
evalIntExp (Mult l r) = do lval <- evalIntExp l
                           rval <- evalIntExp r 
                           return (lval * rval)
evalIntExp (Div l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r 
                          if rval == 0 
                             then throwDivByZero
                             else return (lval `div` rval)

evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r 
                           return (lval && rval)
evalBoolExp (Or l r) = do lval <- evalBoolExp l
                          rval <- evalBoolExp r 
                          return (lval || rval)
evalBoolExp (Lt l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r
                          return (lval < rval)
evalBoolExp (Gt l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r
                          return (lval > rval)
evalBoolExp (Eq l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r
                          return (lval == rval)
-- notacion do 
--
-- dofunc = do x <- action1
--             y <- action2 
--             return x + y
--
-- dofunc = action1 >>= (\x -> action2 >>= (\y -> return x + y))
--
-- dofunc2 = do action1
--              action2
-- dofunc2 = action1 >>= (\_-> action2)
-- dofunc2 = action1 >> action2
