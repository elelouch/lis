import AST
import Control.Monad (liftM,ap)
type Env = [(Variable,Integer)]
env = []
-- Unit type : (), carries no meaningful information

newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> Maybe (a,Env,Integer) }

instance Monad StateErrorTick where
    return x = StateErrorTick (\env -> Just (x,env,0))
    (>>=) monad f = StateErrorTick (\env -> do 
                                (monadVal, monadEnv, opCount) <- (runStateErrorTick monad) env
                                (newMonadVal, newMonadEnv,newMonadCount) <- runStateErrorTick (f monadVal) monadEnv
                                return $ (newMonadVal, newMonadEnv, newMonadCount + opCount))

class Monad m => MonadState m where
    lookfor :: Variable -> m Integer
    update :: Variable -> Integer -> m () -- The monad unit carries no useful information, no computations were done

instance MonadState StateErrorTick where
    lookfor name = StateErrorTick (\env -> maybe Nothing (\mval -> Just (mval,env,0)) (lookfor' env))
                    where lookfor' [] = Nothing
                          lookfor' ((varname,val):vs) = 
                              if name == varname then Just val else lookfor' vs

    update name newval = StateErrorTick (\env -> Just $ ((),update' env,0))
                    where update' [] = [(name,newval)]
                          update' ((varname,val):vs) = 
                              if name == varname 
                                  then (name,newval):vs
                                  else (varname,val) : update' vs

class Monad m => MonadError m where
    throw :: m Integer 

instance MonadError StateErrorTick where
    throw = StateErrorTick (\_ -> Nothing)

class Monad m => MonadTick m where 
    tick :: m ()

instance MonadTick StateErrorTick where 
    tick = StateErrorTick (\env -> Just ((), env, 1))

-- GHC
instance Functor StateErrorTick where 
    fmap = liftM

instance Applicative StateErrorTick where 
    pure = return
    (<*>) = ap


eval Skip = return ()
eval (Assign varname val) = do iexp <- evalIntExp val
                               update varname iexp
eval (Seq stmt1 stmt2) = eval stmt1 >> eval stmt2
eval (If b stmt1 stmt2) = do b <- evalBoolExp b
                             if b 
                                 then eval stmt1
                                 else eval stmt2
eval w@(While b stmt) = eval $ If b (Seq stmt w) Skip

evalIntExp (NVal x) = return x
evalIntExp (Var name) = lookfor name
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
                              then throw
                              else return (lval `div` rval)

evalBoolExp (BTrue) = return True
evalBoolExp (BFalse) = return False
evalBoolExp (Not v) = do bval <- evalBoolExp v
                         return $ not bval
evalBoolExp (Or l r) = do lval <- evalBoolExp l
                          rval <- evalBoolExp r
                          return $ lval || rval
evalBoolExp (And l r) = do lval <- evalBoolExp l
                           rval <- evalBoolExp r
                           return $ lval && rval
evalBoolExp (Gt l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r
                          return $ lval > rval
evalBoolExp (Lt l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r
                          return $ lval < rval
evalBoolExp (Eq l r) = do lval <- evalIntExp l
                          rval <- evalIntExp r
                          return $ lval == rval

evalAst ast = (runStateErrorTick (eval ast)) []
