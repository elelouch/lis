import AST

type Env = [(Variable,Integer)]

newtype StateMonad a = StateMonad { runStateMonad :: Env -> Maybe (a,Env) }
-- recordar que state monad guarda el valor de la ultima computacion
instance Monad StateMonad where
    return x = StateMonad (\env -> (x,env))
    (>>=) sttMonad modifComp = StateMonad (\env -> do 
                                    (lastComp,lastEnv) <- (runStateMonad sttMonad) env
                                    tupleNewCompNewEnv <- (runStateMonad (modifComp lastComputationValue)) lastEnv
                                    return $ Just tupleNewCompNewEnv)
class MonadState m where 
    lookfor :: Variable -> m Int 
    update :: Variable -> Int -> m ()

instance MonadState StateMonad where
    lookfor name = StateMonad (\env -> maybe Nothing (\x -> Just (x, env)) (lookup env))
                                where lookup [] = Nothing
                                      lookup ((varname,val):vs) = 
                                          if varname == name 
                                              then Just (val,env)
                                              else lookup vs
    update name val = StateMonad (\env -> Just $ update env)
        where update [] = [(name,val)]
              update ((varname,val):vs) = 
