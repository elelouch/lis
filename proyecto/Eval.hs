module Eval where
import AST
import Control.Applicative
import Control.Monad (liftM,ap)
import Data.Data

type Env = [(Variable,Type)]
type ProcEnv = [(Variable,Comm)]
data Type = I Integer | L [Integer]

data EnvTracker a = Track a
                  | DivByZero 
                  | ProcedureNotFound 
                  | VarNotFound

newtype StateErrorProcs a = StateErrorProcs { 
    runStateErrorProcs :: ProcEnv -> Env -> EnvTracker (a,ProcEnv,Env,Int)
}

class Monad m => MonadProcs m where 
   lookforProc :: Variable -> m Comm

class Monad m => MonadState m where 
    add :: Variable -> Type -> m ()
    update :: Variable -> Type -> m ()
    lookfor :: Variable -> m Type


class Monad m => MonadTick m where
    tick :: m ()

class Monad m => MonadError m where 
    throwDivByZero :: m a
    throwVarNotFound :: m a
    throwProcedureNotFound :: m a

instance Monad StateErrorProcs where 
    return x = StateErrorProcs (\proceds env -> Track (x,proceds,env,0))
    (>>=) m f = StateErrorProcs (\proceds env -> do 
                            (lastComp,ps,e,invokes) <- (runStateErrorProcs m) proceds env
                            (newComp,ps',e',invokes') <- (runStateErrorProcs (f lastComp)) ps e
                            return $ (newComp,ps',e',invokes + invokes'))

instance MonadProcs StateErrorProcs where 
    lookforProc name =
       StateErrorProcs (\proceds env -> maybe throwProcedureNotFound (\c -> Track (c,proceds,env,0)) (lookforProc' proceds))
                        where lookforProc' [] = Nothing
                              lookforProc' ((pname,c):ps) = 
                                  if pname == name
                                      then Just c 
                                      else lookforProc ps

instance MonadState StateErrorProcs where 
    lookfor name =
       StateErrorProcs (\proceds env -> maybe throwVarNotFound (\v -> Track (v,proceds,env,0)) (lookfor' proceds))
                        where lookfor' [] = Nothing
                              lookfor' ((varname,val):vs) = 
                                  if varname == name
                                      then Just val
                                      else lookforProc vs

instance MonadTick StateErrorProcs where 
    tick = StateErrorProcs (\proceds env -> ((), proceds, env, 1)

instance Monad EnvTracker where 
    return x = Track x
    (>>=) m f = case m of 
                    Track x -> f x
                    DivByZero -> DivByZero
                    ProcedureNotFound -> ProcedureNotFound  
                    VarNotFound -> VarNotFound  

instance Applicative EnvTracker where
    pure = return
    (<*>) = ap

instance Functor EnvTracker where 
    fmap = liftM

instance Applicative StateErrorProcs where
    pure = return
    (<*>) = ap

instance Functor StateErrorProcs where 
    fmap = liftM


instance MonadError StateErrorProcs where
    throwDivByZero = StateErrorProcs(\_ -> DivByZero)
    throwVarNotFound = StateErrorProcs(\_ -> VarNotFound)
    throwProcedureNotFound = StateErrorProcs(\_ -> ProcedureNotFound)

