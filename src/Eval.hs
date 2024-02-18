module Eval(eval) where
import AST
import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)
import Data.Data


data Holder = IntH Int | ListH [Int] deriving Show
type Env = [(Variable,Holder)]
type ProcEnv = [(Variable,Comm)]

data SaveState a = SaveState a
                  | TypeError
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
    lookfor :: Variable -> m Holder
    update :: Variable -> Holder -> m ()

class Monad m => MonadTick m where
    tick :: m ()

class Monad m => MonadError m where 
    throwTypeError :: m a
    throwDivByZero :: m a
    throwVarNotFound :: m a
    throwProcedureNotFound :: m a
    throwTailEmptyList :: m a
    throwIndexOutOfBounds :: m a

-- Recordar que esta monada guarda la ultima operacion realizada
instance Monad StateErrorProcs where 
    return x = StateErrorProcs (\proceds env -> SaveState (x,proceds,env,0))
    (>>=) m f = StateErrorProcs (\astList env -> do 
                            (res,astl,e,inv) <- (runStateErrorProcs m) astList env
                            (res',astl',e',inv') <- (runStateErrorProcs (f res)) astl e
                            return $ (res',astl',e',inv + inv'))


lookfor' _ [] = Nothing
lookfor' name ((varname,val):vs) = 
    if varname == name 
        then Just val 
        else lookfor' name vs

update' name nval [] = [(name,nval)]
update' name nval ((varname,val):vs) = 
    if name == varname 
        then ((name,nval):vs) 
        else (varname,val) : update' name nval vs

instance MonadProcs StateErrorProcs where 
    lookforProc name =
       StateErrorProcs (\proceds env -> maybe ProcedureNotFound (\c -> SaveState (c,proceds,env,0)) (lookfor' name proceds))

instance MonadState StateErrorProcs where 
    lookfor name =
        StateErrorProcs (\proceds env -> maybe VarNotFound (\v -> SaveState (v,proceds,env,0)) (lookfor' name env))
    update name val = 
        StateErrorProcs (\proceds env -> SaveState ((),proceds, update' name val env, 0))

instance MonadTick StateErrorProcs where 
    tick = StateErrorProcs (\proceds env -> SaveState ((), proceds, env, 1))

instance Monad SaveState where 
    return x = SaveState x
    (>>=) m f = case m of 
                    SaveState x -> f x
                    TypeError -> TypeError
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
    throwTypeError = StateErrorProcs(\_ _ -> TypeError)
    throwDivByZero = StateErrorProcs(\_ _ -> DivByZero)
    throwVarNotFound = StateErrorProcs(\_ _-> VarNotFound)
    throwProcedureNotFound = StateErrorProcs(\_ _-> ProcedureNotFound)
    throwIndexOutOfBounds = StateErrorProcs(\_ _-> IndexOutOfBounds)
    throwTailEmptyList = StateErrorProcs(\_ _-> TailEmptyList)

-- eval, que toma una lista de ASTs y devuelve
-- o ProcedureNotFound (falta el main)
-- o una monada a la que se le evaluo el main proc

eval astList = 
    case lookfor' "main" astList of
        Nothing -> ProcedureNotFound
        Just mainProc -> (runStateErrorProcs (evalComm mainProc)) astList []

-- Comandos 

evalComm Skip = return ()
evalComm (Seq c1 c2) = evalComm c1 >> evalComm c2

evalComm (If boolExp c1 c2) = do 
    bool <- evalBoolExp boolExp
    if bool 
        then evalComm c1
        else evalComm c2

evalComm (For c1 bexp c2 forBody) = do 
    evalComm c1
    evalComm (While bexp (Seq forBody c2))

evalComm (While bexp c) = evalComm (If bexp (Seq c (While bexp c)) Skip)
evalComm (Invoke name) = do 
    newAST <- lookforProc name
    tick
    evalComm newAST   

evalComm (Assign name holder) =
    case holder of
        IntExpHolder iexp -> do int <- evalIntExp iexp
                                update name (IntH int)
        ListExpHolder lexp -> do list <- evalListExp lexp
                                 update name (ListH list)

evalListExp (ListVar name) = do
    holder <- lookfor name
    case holder of
        ListH list -> return list
        IntH _ -> throwTypeError

evalListExp (List []) = return []
evalListExp (List (i:is)) = do 
    x <- evalIntExp i
    xs <- evalListExp (List is)
    return (x:xs)

evalListExp (Cons iexp lexp) = do 
    v <- evalIntExp iexp
    vs <- evalListExp lexp
    return (v:vs)

evalListExp (Tail lexp) = do 
    l <- evalListExp lexp
    if null l
        then throwTailEmptyList
        else return (tail l)

evalIntExp (Const n) = return n
evalIntExp (ListIndex iexp lexp) = do 
    index <- evalIntExp iexp
    list <- evalListExp lexp
    if (index >= length list || index < 0)
        then throwIndexOutOfBounds
        else return (list !! index)

evalIntExp (Len lexp) = do 
    list <- evalListExp lexp
    return $ length list

evalIntExp (Var name) = do 
    holder <- lookfor name
    case holder of 
        IntH iexp -> return iexp
        ListH _ -> throwTypeError
        
evalIntExp (Neg iexp) = do
    int <- evalIntExp iexp
    return (negate int)

evalIntExp (Add l r) = do 
    lval <- evalIntExp l
    rval <- evalIntExp r 
    return (lval + rval)

evalIntExp (Sub l r) = do 
    lval <- evalIntExp l
    rval <- evalIntExp r 
    return (lval - rval)

evalIntExp (Mult l r) = do 
    lval <- evalIntExp l
    rval <- evalIntExp r 
    return (lval * rval)

evalIntExp (Div l r) = do
    lval <- evalIntExp l
    rval <- evalIntExp r 
    if rval == 0 
    then throwDivByZero
    else return (lval `div` rval)

evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (And l r) = do 
    lval <- evalBoolExp l
    rval <- evalBoolExp r 
    return (lval && rval)

evalBoolExp (Or l r) = do 
    lval <- evalBoolExp l
    rval <- evalBoolExp r 
    return (lval || rval)

evalBoolExp (Lt l r) = do 
    lval <- evalIntExp l
    rval <- evalIntExp r
    return (lval < rval)

evalBoolExp (Gt l r) = do 
    lval <- evalIntExp l
    rval <- evalIntExp r
    return (lval > rval)

evalBoolExp (Eq l r) = do 
    lval <- evalIntExp l
    rval <- evalIntExp r
    return (lval == rval)
