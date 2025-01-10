module Subst where

import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
import           Expr                (Expr (..))
import           Shared

type Substition = [(String, Expr)]

subst :: Substition -> Expr -> State Integer Expr
subst s (Var x) = return $ fromMaybe (Var x) (lookup x s)
subst _ (Universe k) = return $ Universe k
subst s (Pi x a b) = substAbstraction s (x, a, b) >>= \(x', a', b') -> return (Pi x' a' b')
subst s (Lambda x a b) = substAbstraction s (x, a, b) >>= \(x', a', b') -> return (Lambda x' a' b')
subst s (App e1 e2) = do
    e1' <- subst s e1
    e2' <- subst s e2
    return (App e1' e2')

substAbstraction :: Substition -> (String, Expr, Expr) -> State Integer (String, Expr, Expr)
substAbstraction s (x, t, e) = do
    x' <- refresh x
    t' <- subst s t
    e' <- subst ((x, Var x') : s) e
    return (x', t', e')

refresh :: String -> State Integer String
refresh x = do
    k <- get
    put (k + 1)
    return (x ++ show k)
