module Eval where
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

newtype StateErrorProcs a = StateErrorProcs { 
    runStateErrorProcs :: ProcEnv -> Env -> SaveState (a,ProcEnv,Env,Int)
} 

class Monad m => MonadProcs m where 
   lookforProc :: Variable -> m Comm

class Monad m => MonadState m where 
    updateInt :: Variable -> Int -> m ()
    updateList :: Variable -> Int -> m ()
    lookforInt :: Variable -> m Int
    lookforList :: Variable -> m Int


class Monad m => MonadTick m where
    tick :: m ()

class Monad m => MonadError m where 
    throwDivByZero :: m a
    throwVarNotFound :: m a
    throwProcedureNotFound :: m a
    throwTailEmptyList :: m a
    throwIndexOutOfBounds :: m a

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

evalComm Skip = return ()
evalComm (Assign varname val) = do i <- evalIntExp val
                                   updateInt varname i
evalComm (LAssign varname val) = do i <- evalListExp val
                                    updateList varname i
evalComm (Seq []) = evalComm Skip
evalComm (Seq (c:cs)) = evalComm c >> evalComm (Seq cs)
evalComm (If b c1 c2) = do cond <- evalBoolExp b
                           evalComm (if cond then c1 else c2)

evalComm (While b c) = evalComm (If b (Seq [c, While b c]) Skip)
evalComm (For assign1 b assign2 c) = evalComm (Seq [assign1, While b (Seq [c,assign2])] )

evalListExp (LConst []) = return []
evalListExp (LVar name) = lookforList name
evalListExp (LConst (i:is)) = do x <- evalIntExp i
                                 xs <- evalListExp (LConst is)
                                 return (x:xs)
evalListExp (LCons val list) = do vs <- evalListExp list
                                  v <- evalIntExp val
                                  return (v:vs)
evalListExp (LTail list) = do l <- evalIntExp list
                              if null l
                                  then throwTailEmptyList
                                  else return (tail l)
evalIntExp (LVal index list) = do i <- evalIntExp index
                                  l <- evalListExp list
                                  if (i >= length l || i < 0)
                                      then throwIndexOutOfBounds
                                      else return (l !! i)

evalIntExp (LLen name) = liftM length (lookforList name)
evalIntExp (Const n) = return n
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
