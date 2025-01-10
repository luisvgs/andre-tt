{-# LANGUAGE GADTs #-}
module Expr where

import           BaseType (BaseType (..))

data Expr where
    Var        :: String -> Expr
    Universe   :: Int -> Expr
    Pi         :: String -> Expr -> Expr -> Expr
    Lambda     :: String -> Expr -> Expr -> Expr
    App        :: Expr -> Expr -> Expr
    Gensym     :: String -> Int -> Expr
    Let :: String -> Expr -> Expr ->  Expr -> Expr
    BaseType :: BaseType -> Expr
    Definition  :: String -> Expr -> Expr
    Subtype :: Expr -> Expr -> Expr
    Dummy      :: Expr deriving (Eq)

instance Show Expr where
    show (Var x) = x
    show (Subtype t1 t2) = show t1 ++ " <: " ++ show t2
    show (Definition s e) = s ++ " : " ++ show e
    show (Let x t e u) = "let " ++ x ++ " : " ++ show t ++ " = " ++ show e ++ "; " ++ show u
    show (Universe k) = "Type" ++ show k
    show (Pi x t e) = "Pi (" ++ x ++ " : " ++ show t ++ ") -> " ++ show e
    show (Lambda x t e) = "\\(" ++ x ++ " : " ++ show t ++ ") -> " ++ show e
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Gensym s n) = s ++ show n
    show (BaseType t) = show t
    show Dummy = "Dummy"
