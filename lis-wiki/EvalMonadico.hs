import Control.Monad (liftM,ap)

data Expr = Val Int | Sum Expr Expr | Div Expr Expr deriving Show

newtype Accum a = Ac (a, Int) deriving Show

instance Monad Accum where
    (>>=) (Ac (x,n)) f  = let Ac (x',n') = f x
                           in Ac (x', n' + n)
    return x = Ac (x,0)

instance Applicative Accum where
    pure = return
    (<*>) = ap

instance Functor Accum where 
    fmap = liftM


eval (Val a) = return a
eval (Sum x y) = do x' <- eval x
                    y' <- eval y
                    return (x' + y')
eval (Div x y) = do x' <- eval x
                    y' <- eval y
                    tick
                    return (x' `div` y')
tick = Ac ((),1)

pedro = Div (Div (Sum (Sum (Val 3) (Val 4)) (Sum (Val 2) (Val 1))) (Val 5)) (Val 2)

